---
layout: page
title: CV
---

{% include blog_vars.html %}

<main class="cv-page">
  <nav class="cv-breadcrumb" aria-label="Breadcrumb">
    <a href="{{ site.url }}">kuniga.me</a>
    <span aria-hidden="true">&gt;</span>
    <span>CV</span>
  </nav>

  <section class="cv-section" aria-labelledby="experience-heading">
    <h2 id="experience-heading">Work Experience</h2>

    <article class="cv-entry">
      <figure class="cv-entry-logo">
        <img src="{{ resources }}cv/meta_logo.jpeg" alt="Meta logo" />
      </figure>
      <div class="cv-entry-content">
        <header class="cv-entry-header">
          <h3>Facebook / Meta</h3>
          <span class="cv-entry-date">2012–present</span>
        </header>

        <div class="cv-role">
          <header class="cv-role-header">
            <h4>Stream Processing</h4>
            <span class="cv-role-date">2022–2026</span>
          </header>
          <p>Developed and supported Meta's distributed stream processing engine in C++. Main projects included: implementing stream joins, plugin system to run user code, release system (CI/CD) and observability tools.</p>
          <ul class="cv-skills" aria-label="Stream Processing skills">
            <li>C++</li>
            <li>Python</li>
            <li>Rust</li>
            <li>Stream Processing</li>
            <li>Distributed Systems</li>
          </ul>
        </div>

        <div class="cv-role">
          <header class="cv-role-header">
            <h4>Data Tools</h4>
            <span class="cv-role-date">2012–2021</span>
          </header>
          <p>Developed internal tools for data analyics and observability. One of the creators of the main dashboard platform; created <a href="https://engineering.fb.com/2022/04/26/developer-tools/sql-notebooks/">SQL notebooks</a>, both used widely at Meta as of 2026.</p>
          <ul class="cv-skills" aria-label="Data Tools skills">
            <li>JavaScript</li>
            <li>Hack</li>
            <li>Data Visualization</li>
          </ul>
        </div>
      </div>
    </article>

    <article class="cv-entry">
      <figure class="cv-entry-logo">
        <img src="{{ resources }}cv/cflex_logo.png" alt="CFlex logo" />
      </figure>
      <div class="cv-entry-content">
        <header class="cv-entry-header">
          <h3>CFlex</h3>
          <span class="cv-entry-date">Aug 2011–Sep 2012</span>
        </header>
        <p class="cv-degree">Researcher</p>
        <p>Research and development in railway operations research. Developed algorithms for train scheduling and explored scheduling of railroad crew.</p>
        <ul class="cv-skills" aria-label="CFlex Researcher skills">
          <li>Java</li>
          <li>Operations Research</li>
        </ul>
      </div>
    </article>

    <article class="cv-entry">
      <figure class="cv-entry-logo">
        <img src="{{ resources }}cv/gsoc_logo.svg" alt="Google Summer of Code logo" />
      </figure>
      <div class="cv-entry-content">
        <header class="cv-entry-header">
          <h3>Google Summer of Code</h3>
          <span class="cv-entry-date">2011</span>
        </header>
        <p class="cv-degree">Intern</p>
        <p>I was selected by the BRL-CAD organization to improve their shading system to include support to the Open Shading Language, OSL.</p>
        <ul class="cv-skills" aria-label="Google Summer of Code skills">
          <li>C++</li>
          <li>Computer Graphics</li>
        </ul>
      </div>
    </article>
  </section>

  <section class="cv-section" aria-labelledby="education-heading">
    <h2 id="education-heading">Education</h2>

    <article class="cv-entry">
      <figure class="cv-entry-logo">
        <img src="{{ resources }}cv/np_incompleteness_logo.svg" alt="NP-Incompleteness logo" />
      </figure>
      <div class="cv-entry-content">
        <header class="cv-entry-header">
          <h3>Self-Education</h3>
          <span class="cv-entry-date">2009–present</span>
        </header>

        <div class="cv-role">
          <header class="cv-role-header">
            <h4><a href="{{ site.url }}/blog/">Blog -- NP-Incompleteness</a></h4>
            <span class="cv-role-date">2009–present</span>
          </header>
          <p>I have been studying and writing about computer science and math since 2009. I wrote around 300 posts, posting pretty much every month.</p>
        </div>
      </div>
    </article>

    <article class="cv-entry">
      <figure class="cv-entry-logo">
        <img src="{{ resources }}cv/unicamp_logo.jpeg" alt="Unicamp logo" />
      </figure>
      <div class="cv-entry-content">
        <header class="cv-entry-header">
          <h3>Universidade Estadual de Campinas</h3>
          <span class="cv-entry-date">2009–2011</span>
        </header>
        <p class="cv-degree">MSc Computer Science</p>

        <div class="cv-role">
          <header class="cv-role-header">
            <h4>MSc Researcher</h4>
            <span class="cv-role-date">Jul 2009–Sep 2011</span>
          </header>
          <p>Advisors: Pedro J. Rezende and Cid C. de Souza</p>
          <p>Developed integer linear programming models for a combinatorial optimization problem with applications in cartography. I also developed a UI using Google Maps API to create/visualize some of the real-world instances.</p>
          <p>Five publications resulted from this project. Best dissertation award in the Computer Science department of that year.</p>
          <ul class="cv-skills" aria-label="MSc Researcher skills">
            <li>C++</li>
            <li>CGAL</li>
            <li>XPRESS</li>
            <li>CMake</li>
            <li>Subversion</li>
            <li>JavaScript</li>
            <li>PHP</li>
          </ul>
        </div>

        <div class="cv-role">
          <header class="cv-role-header">
            <h4>Teacher Assistant</h4>
            <span class="cv-role-date">Mar 2010–Jul 2010</span>
          </header>
          <p>Data Structures teacher assistant: responsible for practical classes, including helping students, creation of exercises statements, elaboration of test cases, implementation of the correction program and grading.</p>
          <ul class="cv-skills" aria-label="Teacher Assistant skills">
            <li>Data Structures</li>
          </ul>
        </div>
      </div>
    </article>

    <article class="cv-entry">
      <figure class="cv-entry-logo">
        <img src="{{ resources }}cv/unicamp_logo.jpeg" alt="Unicamp logo" />
      </figure>
      <div class="cv-entry-content">
        <header class="cv-entry-header">
          <h3>Universidade Estadual de Campinas</h3>
          <span class="cv-entry-date">2005–2009</span>
        </header>
        <p class="cv-degree">BSc Computer Engineering</p>

        <div class="cv-role">
          <header class="cv-role-header">
            <h4>Research on Combinatorial Optimization</h4>
            <span class="cv-role-date">2007–2009</span>
          </header>
          <p>Advisor: Orlando Lee</p>
          <p>This work was supported by a two-year grant from FAPESP.</p>
          <ul class="cv-skills" aria-label="Combinatorial Optimization research skills">
            <li>Combinatorial Optimization</li>
          </ul>
        </div>
      </div>
    </article>
  </section>

  <section class="cv-section" aria-labelledby="contests-heading">
    <h2 id="contests-heading">Programming Contests</h2>

    <article class="cv-entry">
      <figure class="cv-entry-logo">
        <img src="{{ resources }}cv/icpc_logo.svg" alt="ICPC logo" />
      </figure>
      <div class="cv-entry-content">
        <header class="cv-entry-header">
          <h3>ICPC World Finals</h3>
          <span class="cv-entry-date">2010</span>
        </header>
        <p>I was part of the team representing Unicamp (with Davi Costa and Marcelo Póvoa) in the ICPC World Finals 2010 in Harbin, China. We solved 4/11 problems and finished in 53th place.</p>
      </div>
    </article>

    <article class="cv-entry">
      <figure class="cv-entry-logo">
        <img src="{{ resources }}cv/gcj_logo.png" alt="Google Code Jam logo" />
      </figure>
      <div class="cv-entry-content">
        <header class="cv-entry-header">
          <h3>Google Code Jam – Latin America</h3>
          <span class="cv-entry-date">2009</span>
        </header>
        <p>I qualified for the Google Code Jam Latin America stage.</p>
      </div>
    </article>

    <article class="cv-entry">
      <figure class="cv-entry-logo">
        <img src="{{ resources }}cv/icpc_logo.svg" alt="ICPC logo" />
      </figure>
      <div class="cv-entry-content">
        <header class="cv-entry-header">
          <h3>ICPC World Finals</h3>
          <span class="cv-entry-date">2008</span>
        </header>
        <p>I was part of the team representing Unicamp (with André Linhares and Paulo Costa) in the ICPC World Finals 2010 in Banff, Canada. We solved solved 2/11 problems and finished in 68th place.</p>
      </div>
    </article>
  </section>
</main>
