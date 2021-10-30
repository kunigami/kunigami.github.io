// Hand-curated hierarchy of tags
const tagsHierarchy = {
    'biology': [
        'evolutionary biology',
        'genetics',
        'human anatomy',
        'molecular biology',
    ],
    'computer science': [
        'approximation algorithms',
        'artificial intelligence',
        'automata',
        'bioinformatics',
        'category theory',
        'combinatorial optimization',
        'compilers',
        'computational complexity',
        'computer architecture',
        'computer network',
        'computer security',
        'data structures',
        'data visualization',
        'databases',
        'distributed systems',
        'graph theory',
        'human-computer interaction',
        'integer programming',
        'numerical optimization',
        'online optimization',
        'operating systems',
        'probabilistic algorithms',
        'quantum computing',
        'software engineering',
    ],
    'electrical engineering': [
        'signal processing',
    ],
    'linguistics': [
        'phonetics',
    ],
    'math': [
        'calculus',
        'combinatorics',
        'functional analysis',
        'linear algebra',
        'number theory',
        'probability',
        'topology',
    ],
    'personal': [
        'conference',
        'project',
        'retrospective',
        'travel',
    ],
    'physics': [
        'acoustics',
    ],
    'programming languages': [
        'agda',
        'bash',
        'c',
        'c++',
        'css',
        'haskell',
        'java',
        'javascript',
        'lisp',
        'ocaml',
        'php',
        'python',
        'r',
        'rust',
        'scala',
    ],
    'software': [
        'chrome',
        'coin-or',
        'd3.js',
        'emacs',
        'git',
        'macos',
        'observable',
    ],
}

const tagsToParent = {};
Object.entries(tagsHierarchy).forEach(([parent, tags]) => {
    tags.forEach(tag => tagsToParent[tag] = parent);
});

function titleize(name) {
    const re = /[ \_]/
    return name.split(re).map(word => capitalize(word)).join(' ');
}

function capitalize(s) {
    return s.charAt(0).toUpperCase() + s.slice(1)
}

class TagsApp extends React.Component {
  render() {
        const {postsByTag} = this.props;

        const uncategorizedTags = Object.keys(postsByTag).sort().filter(tag => !(tag in tagsToParent));
        const tagsHierarchyWithAllTags = {
            ...tagsHierarchy,
            'miscelanea': uncategorizedTags,
        };

        const content = Object.entries(tagsHierarchyWithAllTags).map(([parent, tags]) => {
            console.log(tagsHierarchyWithAllTags);
            const content = tags.filter(tag => tag in postsByTag).map(tag => {
                const postsContent = postsByTag[tag]
                    .sort((postA, postB) => postA.title.localeCompare(postB.title))
                    .map(({title, url}) => <li><a href={url}>{title}</a></li>);
                return (<div>
                    <h3 key={tag}><a name={tag}>{titleize(tag)}</a></h3>
                    <ul>
                        {postsContent}
                    </ul>
                </div>);
            });
          return (
            <div>
                <h2 key={parent}>{titleize(parent)}</h2>
                {content}
            </div>
        );
      });



    return <div> {content} </div>;
  }
}

const encoded_data = document.getElementsByName('tags')[0].content;
const data = JSON.parse(encoded_data);


ReactDOM.render(<TagsApp postsByTag={data} />, document.querySelector("#app"));
