class Page extends React.Component {
  render() {
    return (
      <div id="all">
        <div id="header_wrap" className="outer">
          {this.props.children}
        </div>
      </div>
    );
  }
}

class Header extends React.Component {
  render() {
    return (
      <div id="header_wrap" className="outer">
        <header className="inner">
          <h1 id="project_title">{this.props.title}</h1>
          {this.props.children}
        </header>
      </div>
    );
  }
}

class Body extends React.Component {
  render() {
    return (
      <div id="main_content_wrap" className="outer">
        <section id="main_content" className="inner">
          {this.props.children}
        </section>
      </div>
    );
  }
}

class Section extends React.Component {
  render() {
    return (
      <div id="header_wrap" className="outer">
        <header className="inner">
          <h3>{this.props.title}</h3>
          {this.props.children}
        </header>
      </div>
    );
  }
}
