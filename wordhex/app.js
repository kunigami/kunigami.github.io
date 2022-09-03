const WORDS_LOOKUP = new Set(dictionary);
const INCEPTION = '2022/09/02';
const PAGE_WIDTH = document.querySelector("#app").offsetWidth;
const BOX_SIZE = PAGE_WIDTH / 11 - 11;
const BOX_STYLE = {
  outline: "1px solid",
  width: BOX_SIZE,
  height: BOX_SIZE,
  margin: BOX_SIZE / 10,
}
const KEYS = ["QWERTYUIOP", "ASDFGHJKL", "⏎ZXCVBNM⌫"];
const [GREEN, YELLOW, GRAY] = ['#a6ccaa', '#fffdcb', '#cccccc'];

function toUTC(date) {
  date.setMinutes(date.getMinutes() - date.getTimezoneOffset());
  return date;
}

const MS_IN_DAY = 24 * 60 * 60 * 1000;
function daysBetween(startDate, endDate) {
  return Math.round((toUTC(endDate) - toUTC(startDate)) / MS_IN_DAY);
}

function setCharAt(str, chr, index) {
  return str.substring(0, index) + chr + str.substring(index + 1);
}

class Wordhex extends React.Component {
  componentDidMount() {
    document.addEventListener('keydown', (evt) => this.onChar(evt.key));
  }

  constructor(props) {
    super(props);
    this.state = {
      isActive: true,
      guesses: [],
      inputPosition: 0,
      currGuess: "      ",
      keyMap: {},
    };
  }

  render() {
    return <div className="root">
      {this.state.guesses.map((word, i) => this.renderWord({key: i, word}))}
      {this.renderWord({word: this.state.currGuess, isInput: true})}
      {this.renderKeyboard()}
    </div>;
  }

  renderWord({key, word, isInput}) {
    const isValid = this.isValid(word) || !isInput;
    const matches = this.getMatches(word, this.props.word);
    return <div key={key}>{Array.from(word).map((chr, i) => {
      const color = isInput ? null : ['none', YELLOW, GREEN][matches[i]];
      const isFocused = isInput && i == this.state.inputPosition;
      let style = {
        ...BOX_STYLE,
        backgroundColor: color,
        outline: isFocused ?
          "2px solid blue" :
          (isValid ? BOX_STYLE.outline : "2px solid red"),
      };
      return <span key={i} className="box" style={style}>{chr}</span>;
    })}</div>;
  }

  renderKeyboard() {
    return KEYS.map((row, i) => {
      return <div key={i}>{Array.from(row).map(chr => {
        const keyStatus = this.state.keyMap[chr];
        const style = {
          ...BOX_STYLE,
          backgroundColor: keyStatus == null ?
            'none' :
            [GRAY, YELLOW, GREEN][keyStatus],
        };
        return (
          <span key={chr} className="box" style={style} onClick={(evt) => {
            this.onChar(chr);
            evt.preventDefault();
            evt.stopPropagation();
          }}>{chr}</span>
        );
      })}</div>;
    });
  }

  onChar = (chr) => {
    if (!this.state.isActive) {
      return;
    }
    const {inputPosition, currGuess} = this.state;
    if (chr == 'Enter' || chr == '⏎') {
      this.submit();
    }
    console.log(chr, chr == '⌫');
    if (chr == 'Backspace' || chr == '⌫') {
      if (inputPosition > 0) {
        this.setState({
          currGuess: setCharAt(currGuess, ' ', inputPosition - 1),
          inputPosition: inputPosition - 1,
        });
      }
    }
    if (chr >= 'A' && chr <= 'Z' && inputPosition < this.props.word.length) {
      this.setState({
        currGuess: setCharAt(currGuess, chr.toUpperCase(), inputPosition),
        inputPosition: Math.min(inputPosition + 1, this.props.word.length),
      });
    }
  }

  submit() {
    let {currGuess, guesses, inputPosition, keyMap} = this.state;
    if (inputPosition < this.props.word.length) {
      return;
    }

    const matches = this.getMatches(currGuess, this.props.word);
    const newKeyMap = {...keyMap};
    if (this.isValid(currGuess)) {
      guesses = [...guesses, currGuess];
      for (let i = 0; i < currGuess.length; i++) {
        const chr = currGuess[i];
        newKeyMap[chr] = Math.max(newKeyMap[chr] || 0, matches[i]);
      }
    }

    this.setState({
      isActive: currGuess != this.props.word,
      guesses,
      currGuess: "      ",
      inputPosition: 0,
      keyMap: newKeyMap,
    });
  }

  getMatches(src, dst) {
    const freq = {};
    // perfect matches first
    const matches = Array.from(dst).map((chr, i) => {
      if (src[i] == chr) { return 2; }
      freq[chr] = (freq[chr] || 0) + 1;
      return 0;
    });
    // semi-perfect matches
    return matches.map((match, i) => {
      if (match < 2 && (freq[src[i]] || 0) > 0) {
        freq[src[i]] -= 1;
        return 1;
      }
      return match;
    });
  }

  isValid(word) {
    return this.state.inputPosition < this.props.word.length ||
      WORDS_LOOKUP.has(word);
  }
}

const daySinceStart = daysBetween(new Date(INCEPTION), new Date());
const word = dictionary[daySinceStart % dictionary.length];
ReactDOM.render(
  <Wordhex word={word.toUpperCase()} />,
  document.querySelector("#app")
);
