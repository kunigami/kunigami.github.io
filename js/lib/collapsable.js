
const COLLAPSED_TEXT = "Show proof";
const EXPANDED_TEXT = "Hide proof";

function create_expand_link(body) {
    const link = document.createElement('a');

    link.href = '#';
    link.textContent = COLLAPSED_TEXT;

    link.addEventListener('click', event => {
        event.preventDefault();
        if (link.textContent == COLLAPSED_TEXT) {
            body.style.display = 'block';
            link.textContent = EXPANDED_TEXT;
        } else {
            body.style.display = 'none';
            link.textContent = COLLAPSED_TEXT;
        }
    });
    return link;
}

function add_proofs() {
    let elements = document.getElementsByTagName("proof");
    for (var i = 0; i < elements.length; i++) {
        let element = elements[i];

        const children = element.children;
        element.children = [];
        const proof_body = document.createElement('proof_body');
        const proof_text = document.createElement('b');
        proof_text.textContent = 'Proof.';
        proof_body.appendChild(proof_text);

        // Transfer children to proof_body
        while (element.firstChild) {
            proof_body.appendChild(element.firstChild);
        }
        const text = document.createElement('p');
        text.textContent = element.textContent;
        element.textContent = '';
        proof_body.appendChild(text);

        // We can now make the proof visible since the main
        // child is hidden
        element.style.display = 'block';

        element.appendChild(proof_body);

        const expand_link = create_expand_link(proof_body);
        element.appendChild(expand_link);
    }
}

document.addEventListener('DOMContentLoaded', function() {
    add_proofs();
});
