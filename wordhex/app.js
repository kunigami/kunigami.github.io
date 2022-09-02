const WORD_LEN = 6;

const WORDS_LOOKUP = new Set(dictionary);

const MS_IN_DAY = 24 * 60 * 60 * 1000;
const INCEPTION = '2022/09/02';

const PAGE_WIDTH = document.querySelector("#app").offsetWidth;
const BOX_SIZE = PAGE_WIDTH / 11 - 11;

const BOX_STYLE = {
  alignItems: "center",
  outline: "1px solid",
  width: BOX_SIZE,
  height: BOX_SIZE,
  display: "inline-flex",
  justifyContent: "center",
  margin: BOX_SIZE / 10,
  padding: 0,
  textAlign: "center",
  verticalAlign: "top",
}
const KEYS = [
  "QWERTYUIOP",
  "ASDFGHJKL",
  "⏎ZXCVBNM⌫" //
]
const GREEN = '#a6ccaa';
const YELLOW = '#fffdcb';
const GRAY = '#cccccc'

function toUTC(date) {
  date.setMinutes(date.getMinutes() - date.getTimezoneOffset());
  return date;
}

function daysBetween(startDate, endDate) {
  return Math.round((toUTC(endDate) - toUTC(startDate)) / MS_IN_DAY);
}

function setCharAt(str, chr, index) {
  if(index >= str.length) {
    return str;
  }
  return str.substring(0, index) + chr + str.substring(index + 1);
}

class Wordhex extends React.Component {

  componentDidMount() {
    document.addEventListener('keydown', this.onKeyDown);
  }

  constructor(props) {
    super(props);
    this.state = {
      isActive: true,
      guesses: [
      ],
      inputPosition: 0,
      currentGuess: "      ",
      keyMap: {},
    };
    this.inputBoxes = Array(WORD_LEN);
  }

  render() {
    const outerStyle = {
      fontFamily: "Inconsolata",
      display: "flex",
      justifyContent: "center",
    };
    const innerStyle = {
      display: "flex",
      alignItems: "center",
      flexDirection: "column",
    };

    const words = this.state.guesses.map(guess => {
      return this.renderWord({word: guess, showMatch: true});
    });

    return <div style={outerStyle}>
      <div style={innerStyle}>
      {words}
      {this.renderWord({word: this.state.currentGuess, isInput: true})}
      {this.renderKeyboard()}
      </div>
    </div>;
  }

  renderWord({word, isInput}) {
    const boxes = [];
    const isValid = this.isValid(word) || !isInput;
    const matches = this.getMatches(word);
    for (let i = 0; i < word.length; i++) {
      let color;
      if (!isInput) {
        color = ['none', YELLOW, GREEN][matches[i]];
      }
      let style = {
        ...BOX_STYLE,
        backgroundColor: color,
      };

      if (!isValid) {
        style.outline = "2px solid red";
      } else if (isInput && i == this.state.inputPosition) {
        style.outline = "2px solid blue";
      }

      boxes.push(<span style={style}>{word[i]}</span>);
    }
    return <div>{boxes}</div>;
  }

  renderKeyboard() {
    const rows = [];
    for (let i = 0; i < KEYS.length; i++) {
      const boxes = [];
      for (let j = 0; j < KEYS[i].length; j++) {
        const chr = KEYS[i].charAt(j);
        let color = 'none';
        if (this.state.keyMap[chr] != null) {
          color = [GRAY, YELLOW, GREEN][this.state.keyMap[chr] || 0];
        }

        let style = {
          ...BOX_STYLE,
          backgroundColor: color,
        };
        boxes.push(<span style={style} onClick={() => this.onCharEntered(KEYS[i].charAt(j))}>{KEYS[i].charAt(j)}</span>);
      }
      rows.push([<div>{boxes}</div>]);
    }
    return rows;
  }

  handleChange(e) {
    this.setState({ value: e.target.value });
  }

  onKeyDown = (evt) => {
    this.onCharEntered(evt.key);
  }

  onCharEntered = (chr) => {
    if (!this.state.isActive) {
      return;
    }
    const {inputPosition} = this.state;

    if (chr == 'Enter' || chr == '⏎') {
      this.submit();
      return;
    }
    if (chr == 'Backspace' || chr == '⌫') {
      if (inputPosition > 0) {
        this.setState({
          currentGuess: setCharAt(this.state.currentGuess, ' ', inputPosition - 1)});

        this.setState({
          inputPosition: inputPosition - 1,
        });
      }
      return;
    }

    if (chr.length != 1) {
      return;
    }

    this.setState({
      currentGuess: setCharAt(this.state.currentGuess, chr.toUpperCase(), inputPosition)});

    if (inputPosition < WORD_LEN) {
      this.setState({
        inputPosition: inputPosition + 1,
      });
    }
  }

  submit() {
    const {currentGuess, keyMap} = this.state;
    const matches = this.getMatches(currentGuess);

    if (currentGuess == this.props.word) {
      this.setState({
        isActive: false
      });
    }

    const newKeyMap = {...keyMap};
    let {guesses} = this.state;
    if (this.isValid(currentGuess)) {
      guesses = [...this.state.guesses, currentGuess];
      for (let i = 0; i < currentGuess.length; i++) {
        const chr = currentGuess[i];
        newKeyMap[chr] = Math.max(newKeyMap[chr] || 0, matches[i]);
      }
    }

    this.setState({
      guesses,
      currentGuess: "      ",
      inputPosition: 0,
      keyMap: newKeyMap,
    });
  }

  getMatches(src) {
    const dst = this.props.word;
    const freq = {};
    const matches = Array(src.length);
    // perfect matches first
    for (let i = 0; i < src.length; i++) {
      if (src[i] == dst[i]) {
        matches[i] = 2;
      } else {
        freq[dst[i]] = (freq[dst[i]] || 0) + 1;
      }
    }
    // semi-perfect matches
    for (let i = 0; i < src.length; i++) {
      if (src[i] == dst[i]) {
        continue;
      }
      const f = freq[src[i]] || 0;
      if (f > 0) {
        freq[src[i]] -= 1;
        matches[i] = 1;
      } else {
        matches[i] = 0;
      }
    }
    return matches;
  }

  // 0 - no match (gray)
  // 1 - partial match
  // 2 - match
  getMatchType(chr, pos, cnt) {
    for (let i = 0; i < this.props.word.length; i++) {
      if (chr == this.props.word[i]) {
        cnt -= 1;
        if (pos == i) {
          return 2;
        }
        if (cnt == 0) {
          return 1;
        }
      }
    }
    return 0;
  }

  isValid(word) {
    return this.state.inputPosition < WORD_LEN || WORDS_LOOKUP.has(word);
  }
}




const wordIndex = daysBetween(new Date(INCEPTION), new Date()) % dictionary.length;
const word = dictionary[wordIndex];



ReactDOM.render(
  <Wordhex word={word.toUpperCase()} />,
  document.querySelector("#app")
);
