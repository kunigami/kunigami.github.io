import importlib.util
import io
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

    def test_test_mode_always_sends_only_to_recipient_without_database_access(self):
        post = {"id": "latest-post"}
        with (
            patch.object(self.sender, "get_latest_post", return_value=post),
            patch.object(self.sender, "send_email") as send_email,
            patch.object(self.sender, "get_subscribers") as get_subscribers,
            patch.object(self.sender, "mark_sent") as mark_sent,
            patch.object(self.sender, "subscribers_table") as table,
        ):
            for _ in range(2):
                send_email.reset_mock()
                self.sender.main(["--test-recipient", " me@example.com "])
                send_email.assert_called_once_with("me@example.com", post)
            get_subscribers.assert_not_called()
            mark_sent.assert_not_called()
            self.assertEqual(table.mock_calls, [])

    def test_invalid_test_recipient_does_not_fall_back_to_subscribers(self):
        cases = [
            ["--test-recipient"],
            ["--test-recipient", ""],
            ["--test-recipient", "   "],
            ["--test-recipient", "not-an-email"],
            ["--test-recipient", "one@example.com,two@example.com"],
        ]
        with (
            patch.object(self.sender, "get_latest_post") as get_latest_post,
            patch.object(self.sender, "get_subscribers") as get_subscribers,
            patch.object(self.sender, "send_email") as send_email,
            patch("sys.stderr", new_callable=io.StringIO),
        ):
            for argv in cases:
                with self.subTest(argv=argv), self.assertRaises(SystemExit) as error:
                    self.sender.main(argv)
                self.assertEqual(error.exception.code, 2)
            get_latest_post.assert_not_called()
            get_subscribers.assert_not_called()
            send_email.assert_not_called()

    def test_normal_mode_skips_sent_posts_and_marks_new_sends(self):
        post = {"id": "latest-post"}
        subscribers = [
            {"email": "already@example.com", "last_sent_post": post["id"]},
            {"email": "new@example.com"},
        ]
        with (
            patch.object(self.sender, "get_latest_post", return_value=post),
            patch.object(self.sender, "get_subscribers", return_value=subscribers),
            patch.object(self.sender, "send_email") as send_email,
            patch.object(self.sender, "mark_sent") as mark_sent,
        ):
            self.sender.main([])
            send_email.assert_called_once_with("new@example.com", post)
            mark_sent.assert_called_once_with("new@example.com", post["id"])

    def test_preprocess_replaces_multiple_spoilers(self):
        content = (
            "<p>Before<spoiler>First secret</spoiler>between</p>"
            "<spoiler>Second secret</spoiler><p>After</p>"
        )
        self.assertEqual(
            self.sender.preprocess_content(content),
            "<p>Before[Content deleted due to spoilers. "
            "Visit the post on the blog to see it.]between</p>"
            "[Content deleted due to spoilers. "
            "Visit the post on the blog to see it.]<p>After</p>",
        )

    def test_preprocess_replaces_spoilers_with_nested_html(self):
        content = (
            "<p>Public</p><spoiler><div>Secret <strong>answer</strong>"
            '<a href="/secret">details</a><spoiler>Nested secret</spoiler>'
            "</div></spoiler><p>More public</p>"
        )
        self.assertEqual(
            self.sender.preprocess_content(content),
            "<p>Public</p>[Content deleted due to spoilers. "
            "Visit the post on the blog to see it.]<p>More public</p>",
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
        html = BeautifulSoup(body["Html"]["Data"], "html.parser")
        links = html.find_all("a")
        self.assertEqual(
            [link["href"] for link in links],
            [post["url"], article_url, unsubscribe_url],
        )
        title_link = html.find("h1").find("a")
        self.assertEqual(title_link["href"], post["url"])
        self.assertEqual(title_link.get_text(), post["title"])
        self.assertNotIn("Read online", html.get_text())
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
                self.assertNotIn("<spoiler", body[representation]["Data"])
                self.assertIn(
                    "[Content deleted due to spoilers. "
                    "Visit the post on the blog to see it.]",
                    body[representation]["Data"],
                )
        self.assertIn("<p>Public text</p>", body["Html"]["Data"])
        self.assertNotIn("<p>", body["Text"]["Data"])
        self.assertEqual(post["content_html"], original_content)


if __name__ == "__main__":
    unittest.main()
