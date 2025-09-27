import json
import os

from pathlib import Path

assert (
    Path.cwd().name == "blog-draft-posts"
), f"This must be run at the root directory. Got {Path.cwd()}"


def normalize(name):
    return name.lower().replace(" ", "_")


def get_url(path):
    pathstr = str(path).replace("_nature", "nature")
    return "{{site.url}}/" + pathstr


HIERARCHY = {
    "Nature": "Kingdom",
    "Kingdom": "Phylum",
    "Phylum": "Class",
    "Class": "Order",
    "Order": "Family",
    "Family": "Genus",
    "Genus": "Species",
}


def generate_page(path, name, content, node_type):
    if node_type == "Species":
        return

    print(path / name)

    child_type = HIERARCHY[node_type]
    name_norm = normalize(name)
    subpages = content["children"]

    subpages_links = []
    for subpage_name, subpage_info in subpages.items():
        subpage_title = subpage_name
        if "alias" in subpage_info:
            alias = subpage_info["alias"]
            subpage_title = f"{subpage_title} ({alias})"

        subpage_name_norm = normalize(subpage_name)
        subpage_path = path / name_norm / (subpage_name_norm + ".html")
        link = f"* [{subpage_title}]({get_url(subpage_path)})"
        subpages_links.append(link)
    subpages_content = "\n".join(subpages_links)

    if name_norm == "_nature":
        title = "Nature"
    else:
        title = f"{node_type}: {name}"

    content = f"""
---
layout: nature
title: "{title}"
---

{{% include blog_vars.html %}}

## {child_type}
{subpages_content}

"""
    content = content.lstrip()

    if name_norm == "_nature":
        file_path = "nature/index.md"
    else:
        file_path = path / f"{name_norm}.md"

    with open(file_path, "w") as file:
        file.write(content)

    # Create directory for children
    children_dir = path / name_norm
    os.makedirs(children_dir, exist_ok=True)

    for subpage_name, subpage in subpages.items():
        generate_page(path / name_norm, subpage_name, subpage, child_type)


with open("resources/nature/data.json", "r") as file:
    content = file.read()
    json_content = json.loads(content)
    print(json_content)

    generate_page(Path(""), "_nature", json_content, "Nature")

    # os.makedirs("my_directory", exist_ok=True)
