function titleize(str) {
  return str
    .split(/[\s_]+/) // split on whitespace
    .map(word => word.charAt(0).toUpperCase() + word.slice(1))
    .join(" ");
}

function removePrefix(str, prefix) {
  return str.startsWith(prefix) ? str.slice(prefix.length) : str;
}

function removeSuffix(str, suffix) {
  return str.endsWith(suffix) ? str.slice(0, -suffix.length) : str;
}

class Breadcrumb extends HTMLElement {
    constructor() {
        super();
    }

    connectedCallback() {
        this.render();
    }

    render() {
        const base = this.getAttribute('base');
        let path = this.getAttribute('path');
        path = removePrefix(path, "/");

        const parts = path.split("/")
        const links_container = document.createElement('p');

        const root_link = document.createElement('a');
        root_link.href = base;
        root_link.innerHTML = "kuniga.me";
        links_container.appendChild(root_link);

        let url_path = base;
        let is_first = true;
        for (let part of parts) {
            const delim = document.createElement('span');
            delim.innerHTML = ' > ';
            links_container.appendChild(delim);

            if (part == "_nature") {
                part = "nature";
            }

            url_path = url_path + '/' + part
            const url = url_path + (part == "nature" ? "" : ".html");
            const link = document.createElement('a');
            link.href = url;

            part = removeSuffix(part, ".html");

            link.innerHTML = titleize(part);
            links_container.appendChild(link);
        }

        this.appendChild(links_container);
    }
}

customElements.define('page-breadcrumb', Breadcrumb);
