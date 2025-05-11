
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
        const url = `{{site.url}}/books/${vanity}`;
        const a = document.createElement('a');
        a.setAttribute('href', url);
        a.textContent = title;
        a.style.textDecoration = 'none';
        a.style.color = '#358497';
        a.style.textShadow = 'none';

        const td = document.createElement('td');
        td.appendChild(a);
        this.shadowRoot.appendChild(td);
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
        const path = `https://www.kuniga.me/resources/books/${file}`;
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

class BookTable extends HTMLElement {
    constructor() {
      super();
      this.attachShadow({ mode: 'open' });
    }

    connectedCallback() {
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

    text(msg) {
        const span = document.createElement('span');
        span.textContent = msg;
        return span;
    }

    render() {
        const headers = [
            ['Title', '25%'],
            ['Authors', '25%'],
            ['Rating', '20%'],
            ['Category', '15%'],
            ['Year', '10%'],
            ['Cover','10%'],
        ];
        const rows = JSON.parse(this.getAttribute('data') || '[]');
        console.log(rows);

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
          console.log(h)
          th.textContent = h[0].trim();
          th.style.border = '1px solid #ccc';
          th.style.padding = '8px';
          th.style.background = '#f0f0f0';
          th.style.width = h[1];
          trHead.appendChild(th);
        });
        thead.appendChild(trHead);
        table.appendChild(thead);

        // Create tbody
        const tbody = document.createElement('tbody');
        rows.forEach(row => {
            const tr = document.createElement('tr');

            const title = this.td({width: '30%'});
            const titleInner = document.createElement('book-link');
            titleInner.setAttribute('title', row['title']);
            titleInner.setAttribute('vanity', row['vanity']);
            title.appendChild(this.center(titleInner));

            const author = this.td({width: '30%'});
            author.appendChild(this.center(this.text(row['author'])));

            const rating = this.td({width: '10%'});
            const ratingInner = document.createElement('book-rating');
            ratingInner.setAttribute('rating', row['rating']);
            rating.appendChild(this.center(ratingInner));

            const cover = this.td({width: '10%'});
            const coverInner = document.createElement('book-cover');
            coverInner.setAttribute('image', row['image']);
            cover.appendChild(this.center(coverInner));

            const year = this.td({width: '10%'});
            year.appendChild(this.center(this.text(row['year'])));

            const category = this.td({width: '10%'});
            category.appendChild(this.center(this.text(row['category'])));

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
