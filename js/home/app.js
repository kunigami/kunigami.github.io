class Home extends React.Component {
  render() {
    return (
      <Page>
        <Header title={"Guilherme Kunigami"}>
          {"I'm"} a Software Engineer based in the San Francisco Bay Area,
          currently working with{" "}
          <a href="https://www.linkedin.com/in/kunigami">web development</a>. I
          like to learn and build things.
        </Header>
        <Body>
          <Section title="Links">
            <ul>
              <li>
                <a href="https://github.com/kunigami" target="_blank">Github</a>
              </li>
              <li>
                <a href="http://kuniga.me/blog" target="_blank">Blog</a>
              </li>
              <li>
                <a href="https://twitter.com/kunigami" target="_blank">Twitter</a>
              </li>
              <li>
                <a href="http://www.informatik.uni-trier.de/~ley/pers/hd/k/Kunigami:Guilherme" target="_blank">Papers</a>
              </li>
            </ul>
          </Section>
        </Body>
      </Page>
    );
  }
}

ReactDOM.render(<Home />, document.querySelector("#app"));
