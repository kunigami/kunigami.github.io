const GUESS = 0;
const SUBTREE = 1;

class BullsAndCows extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      items: [],
      node: props.decisionTree
    };
  }

  render() {
    const itemsReact = this.state.items.map((item, index) => {
      return (
        <Item key={index}>
          Guessed: {this.renderGuess(item.guess)}, result: {item.result}
        </Item>
      );
    });

    let solvedMessage;
    let plugMessage;
    if (!this.isSolved()) {
      itemsReact.push(
        <Item key={"current"}>
          Guess: {this.renderGuess(this.state.node[GUESS])}
        </Item>
      );
    } else {
      solvedMessage = (
        <Message>
          Solved! The secret is: {this.renderGuess(this.state.node[GUESS])}
        </Message>
      );
    }

    return (
      <div>
        <Header title={"A Bulls and Cows Solver"}>
          <p>
            If you are not familiar with this game, check this{" "}
            <a
              href="https://en.wikipedia.org/wiki/Bulls_and_Cows"
              target="_blank"
            >
              Wikipedia article
            </a>.
          </p>
          <p>
            <b>How to use the solver.</b> Think of a 4-digit number with no
            repeated digits. Then choose the outcome (x, y) in the dropdown
            below, where x is the number of digits the solver got right AND in
            the right position (bulls), and y be the number of digits it got
            right but in the wrong position (cows).
          </p>
          <p className="margin_top_xxxl">
            If you are interested in the implementation of the solver, check our{" "}
            <a
              href={"https://www.kuniga.me/blog/2018/06/04/bulls-and-cows/"}
              target="_blank"
            >
              blog post
            </a>.
          </p>
        </Header>
        <Body>
          <ol>{itemsReact}</ol>
          {solvedMessage}
          <div>
            {this.renderOptions()}
            <button className="margin_left_l" onClick={this.resetGuesses}>
              Reset
            </button>
          </div>
        </Body>
      </div>
    );
  }

  handleChange = event => {
    const selection = event.target.value;
    if (selection === "none") {
      return;
    }
    this.setState(() => {
      const currentGuess = this.state.node[GUESS];
      const items = this.state.items;
      items.push({ guess: currentGuess, result: selection });
      const nextNode = this.state.node[SUBTREE][selection];
      return { items, node: nextNode };
    });
  };

  renderGuess(guess) {
    return JSON.stringify(guess);
  }

  isSolved() {
    return (
      !this.state.node[SUBTREE] ||
      Object.keys(this.state.node[SUBTREE]).length == 0
    );
  }

  renderOptions() {
    const subtree = this.state.node[SUBTREE];
    // Leaf node, meaning we found the solution
    if (!subtree) {
      return null;
    }

    const options = [<option value={"none"}>Select outcome</option>].concat(
      Object.keys(subtree).map(value => <option value={value}>{value}</option>)
    );
    return (
      <select
        style={{ marginTop: 20 }}
        value={"none"}
        onChange={this.handleChange}
      >
        {options}
      </select>
    );
  }

  resetGuesses = () => {
    this.setState({ items: [], node: this.props.decisionTree });
  };
}

function Item({ children }) {
  const style = {
    fontFamily: "Monaco"
  };
  return <li style={style}>{children}</li>;
}

function Message({ children }) {
  const style = {
    fontFamily: "Monaco"
  };
  return <div style={style}>{children}</div>;
}

ReactDOM.render(
  <BullsAndCows decisionTree={decisionTree} />,
  document.querySelector("#app")
);
