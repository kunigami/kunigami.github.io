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
          <Section title="Projects">
            <p>
              Most of my projects are on{" "}
              <a href="https://github.com/kunigami" target="_blank">
                Github
              </a>. Here are some of them:
            </p>

            <ul>
              <li>
                Data structures in OCaml (<a href="https://github.com/kunigami/ocaml-data-structures">
                  github
                </a>)
              </li>
              <li>
                A Raspberry Pi monitoring system (<a href="https://kunigami.blog/2017/01/15/domestic-server-using-raspberry-pi/">
                  post
                </a>)
              </li>
              <li>
                Bulls and Cows solver (<a href="/bulls_and_cows/">link</a>)
              </li>
            </ul>
          </Section>
          <Section title="Writing">
            <p>
              I like to write about technical subjects. I have been{" "}
              <a href="http://kunigami.blog/" target="_blank">
                blogging
              </a>{" "}
              since 2009 (I used to blog in{" "}
              <a href="https://kuniga.wordpress.com/" target="_blank">
                Portuguese
              </a>{" "}
              until 2012).
            </p>

            <p>
              {"I'm"} also on Twitter{" "}
              <a href="https://twitter.com/kunigami">@kunigami</a>, but I
              {" don't "}
              write much there. Mostly retweeting or saving stuff to read later
              (aka never).
            </p>

            <p>
              Back in grad school I did research on Combinatorial Optimization
              and{" "}
              <a href="http://www.informatik.uni-trier.de/~ley/pers/hd/k/Kunigami:Guilherme">
                published some papers
              </a>. I wish I could do more of it.
            </p>
          </Section>
        </Body>
      </Page>
    );
  }
}

ReactDOM.render(<Home />, document.querySelector("#app"));
