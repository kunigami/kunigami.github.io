import importlib.util
from pathlib import Path
import unittest
from unittest.mock import Mock, patch

from bs4 import BeautifulSoup


class SenderTest(unittest.TestCase):
    def setUp(self):
        self.ses = Mock()
        spec = importlib.util.spec_from_file_location(
            "newsletter_send", Path(__file__).with_name("send.py")
        )
        self.sender = importlib.util.module_from_spec(spec)
        # The sender creates AWS clients at import time.
        with (
            patch("boto3.client", return_value=self.ses),
            patch("boto3.resource"),
        ):
            spec.loader.exec_module(self.sender)

    def test_preprocess_preserves_content_without_spoilers(self):
        content = '<p>Public <strong>text</strong> &amp; <a href="/post">link</a>.</p>'
        self.assertEqual(self.sender.preprocess_content(content), content)

    def test_preprocess_empty_content(self):
        self.assertEqual(self.sender.preprocess_content(""), "")

    def test_preprocess_removes_multiple_spoilers(self):
        content = (
            "<p>Before<spoiler>First secret</spoiler>between</p>"
            "<spoiler>Second secret</spoiler><p>After</p>"
        )
        self.assertEqual(
            self.sender.preprocess_content(content),
            "<p>Beforebetween</p><p>After</p>",
        )

    def test_preprocess_removes_spoilers_with_nested_html(self):
        content = (
            "<p>Public</p><spoiler><div>Secret <strong>answer</strong>"
            '<a href="/secret">details</a><spoiler>Nested secret</spoiler>'
            "</div></spoiler><p>More public</p>"
        )
        self.assertEqual(
            self.sender.preprocess_content(content),
            "<p>Public</p><p>More public</p>",
        )

    def test_send_email_preserves_direct_links(self):
        article_url = "https://www.kuniga.me/books/2021/04/01/the-pleasure-of-finding-things-out.html"
        unsubscribe_url = "https://example.com/unsubscribe?email=reader&sig=test"
        post = {
            "title": "Test post",
            "url": "https://www.kuniga.me/test",
            "content_html": f'<p><a href="{article_url}">Book</a></p>',
        }
        with patch.object(
            self.sender, "make_unsubscribe_url", return_value=unsubscribe_url
        ):
            self.sender.send_email("reader@example.com", post)

        self.ses.send_email.assert_called_once()
        body = self.ses.send_email.call_args.kwargs["Content"]["Simple"]["Body"]
        links = BeautifulSoup(body["Html"]["Data"], "html.parser").find_all("a")
        self.assertEqual(
            [link["href"] for link in links],
            [article_url, post["url"], unsubscribe_url],
        )
        for link in links:
            with self.subTest(url=link["href"]):
                self.assertTrue(link.has_attr("ses:no-track"))
        self.assertIn(post["url"], body["Text"]["Data"])
        self.assertIn(unsubscribe_url, body["Text"]["Data"])

    def test_send_email_excludes_spoilers_from_both_bodies(self):
        original_content = (
            "<p>Public text</p><spoiler>Secret <strong>answer</strong></spoiler>"
        )
        post = {
            "title": "Test post",
            "url": "https://www.kuniga.me/test",
            "content_html": original_content,
        }
        with patch.object(
            self.sender, "make_unsubscribe_url", return_value="https://example.com/unsubscribe"
        ):
            self.sender.send_email("reader@example.com", post)

        self.ses.send_email.assert_called_once()
        body = self.ses.send_email.call_args.kwargs["Content"]["Simple"]["Body"]
        for representation in ("Html", "Text"):
            with self.subTest(representation=representation):
                self.assertIn("Public text", body[representation]["Data"])
                self.assertNotIn("Secret", body[representation]["Data"])
                self.assertNotIn("answer", body[representation]["Data"])
                self.assertNotIn("spoiler", body[representation]["Data"])
        self.assertIn("<p>Public text</p>", body["Html"]["Data"])
        self.assertNotIn("<p>", body["Text"]["Data"])
        self.assertEqual(post["content_html"], original_content)


if __name__ == "__main__":
    unittest.main()
