class Page extends React.Component {
  render() {
    return (
      <div id="page">
        {this.props.children}
      </div>
    );
  }
}

class Header extends React.Component {
  render() {
    return (
      <div id="header_wrap">
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
      <section id="main_content" className="inner">
        {this.props.children}
      </section>
    );
  }
}

class Section extends React.Component {
  render() {
    return (
      <div>
        <header className="inner">
          <h3>{this.props.title}</h3>
          {this.props.children}
        </header>
      </div>
    );
  }
}
