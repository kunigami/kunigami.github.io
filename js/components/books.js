
class BookLink extends HTMLElement {
    constructor() {
        super();
        this.attachShadow({ mode: 'open' });
    }

    connectedCallback() {
        this.render();
    }

    render() {
        const title = this.getAttribute('title');
        const vanity = this.getAttribute('vanity');
        const baseUrl = this.getAttribute('base_url');
        const url = `${baseUrl}/books/${vanity}`;
        const a = document.createElement('a');
        a.setAttribute('href', url);
        a.textContent = title;
        a.style.textDecoration = 'none';
        a.style.color = '#358497';
        a.style.textShadow = 'none';

        this.shadowRoot.appendChild(a);
    }
}
customElements.define('book-link', BookLink);

class BookCover extends HTMLElement {
    constructor() {
        super();
        this.attachShadow({ mode: 'open' });
    }

    connectedCallback() {
        this.render();
    }

    render() {
        const file = this.getAttribute('image');
        const baseUrl = this.getAttribute('base_url');
        const path = `${baseUrl}/resources/books/${file}`;
        const img = document.createElement('img');
        img.setAttribute('src', path);
        img.style.height = '100px';
        img.style.marginTop = '3px';

        const td = document.createElement('td');
        td.appendChild(img);

        this.shadowRoot.appendChild(td);
    }
}
customElements.define('book-cover', BookCover);

class BookRating extends HTMLElement {
    constructor() {
        super();
        this.attachShadow({ mode: 'open' });
    }

    connectedCallback() {
        this.render();
    }

    render() {
        const rating = this.getAttribute('rating');
        let stars = '';
        for (let i = 0; i < rating; i++) {
            stars += '⭐';
        }

        const td = document.createElement('td');
        td.textContent = stars;

        this.shadowRoot.appendChild(td);
    }
}
customElements.define('book-rating', BookRating);

function strCmp(a, b) {
    return a.toLowerCase().localeCompare(b.toLowerCase());
}

function intCmp(a, b) {
    return a - b;
}

class BookTable extends HTMLElement {
    constructor() {
      super();
      this.rows = null;
      this.sortBy = null;
      this.asc = true;
      this.attachShadow({ mode: 'open' });
    }

    async connectedCallback() {
        const response = await fetch('data.json');
        this.rows = await response.json();
        this.render();
    }

    td(options) {
        const td = document.createElement('td');
        td.style.width = options['width'];
        return td;
    }

    center(child) {
        const div = document.createElement('div');
        div.style.display = 'flex';
        div.style.justifyContent = 'center';
        div.appendChild(child);
        return div;
    }

    cell(child) {
        const div = document.createElement('div');
        div.style.marginRight = '10px';
        div.appendChild(child);
        return div;
    }

    text(msg) {
        const span = document.createElement('span');
        span.textContent = msg;
        return span;
    }

    getSortedRows() {
        let cmp = null;
        let key;
        switch (this.sortBy) {
            case 'Title':
                cmp = strCmp;
                key = 'title';
                break;

            case 'Author':
                cmp = strCmp;
                key = 'author';
                break;

            case 'Category':
                cmp = strCmp;
                key = 'category';
                break;

            case 'Rating':
                cmp = intCmp;
                key = 'rating';
                break;

            case 'Year Read':
                    cmp = intCmp;
                    key = 'year';
                    break;
        }

        if (!cmp) {
            return this.rows;
        }

        const clone = [...this.rows];
        let sign = this.asc ? 1 : -1;
        clone.sort((a, b) => cmp(a[key], b[key]) * sign);
        return clone;
    }

    render() {
        // title | width | sortable
        const headers = [
            ['Title', '25%', true],
            ['Author', '25%', true],
            ['Rating', '20%', true],
            ['Category', '15%', true],
            ['Year Read', '10%', true],
            ['Cover','10%', false],
        ];

        let rows = this.getSortedRows();

        const table = document.createElement('table');
        table.className = 'books-index';
        table.style.borderCollapse = 'collapse';
        table.style.width = '120%';
        table.style.tableLayout = 'fixed';

        // Create header
        const thead = document.createElement('thead');
        const trHead = document.createElement('tr');
        headers.forEach(h => {
            const th = document.createElement('th');
            if (h[2]) {
                th.addEventListener('click', (e) => {
                    if (this.sortBy) {
                        if (this.asc) {
                            this.asc = false;
                        } else {
                            this.sortBy = null;
                        }
                    } else {
                        this.sortBy = h[0];
                        this.asc = true;
                    }
                    this.render();
                });

                th.style.cursor = 'pointer';
            }
            th.textContent = h[0].trim();
            if (this.sortBy == h[0]) {
                if (this.asc) {
                    th.textContent += ' ⬇️';
                } else {
                    th.textContent += ' ⬆️';
                }
            }

            th.style.border = '1px solid #ccc';
            th.style.padding = '8px';
            th.style.background = '#f0f0f0';
            th.style.width = h[1];
            trHead.appendChild(th);
        });
        thead.appendChild(trHead);
        table.appendChild(thead);
        const baseUrl = this.getAttribute('base_url');

        // Create tbody
        const tbody = document.createElement('tbody');
        rows.forEach(row => {
            const tr = document.createElement('tr');

            const title = this.td({width: '30%'});
            const titleInner = document.createElement('book-link');
            titleInner.setAttribute('title', row['title']);
            titleInner.setAttribute('vanity', row['vanity']);
            titleInner.setAttribute('base_url', baseUrl);
            title.appendChild(this.cell(titleInner));

            const author = this.td({width: '30%'});
            author.appendChild(this.cell(this.text(row['author'])));

            const rating = this.td({width: '10%'});
            const ratingInner = document.createElement('book-rating');
            ratingInner.setAttribute('rating', row['rating']);
            rating.appendChild(this.cell(ratingInner));

            const category = this.td({width: '10%'});
            category.appendChild(this.cell(this.text(row['category'])));

            const year = this.td({width: '10%'});
            year.appendChild(this.cell(this.text(row['year'])));

            const cover = this.td({width: '10%'});
            const coverInner = document.createElement('book-cover');
            coverInner.setAttribute('image', row['image']);
            coverInner.setAttribute('base_url', baseUrl);
            cover.appendChild(this.center(coverInner));

            tr.appendChild(title);
            tr.appendChild(author);
            tr.appendChild(rating);
            tr.appendChild(category);
            tr.appendChild(year);
            tr.appendChild(cover);

            tbody.appendChild(tr);
        });
        table.appendChild(tbody);

        // Clear previous and insert
        this.shadowRoot.innerHTML = '';
        this.shadowRoot.appendChild(table);
    }
}

customElements.define('book-table', BookTable);
