# Cline Workspace Rules: Documentation (JavaDocs & Maven Site)

These rules define how documentation (Maven Site + JavaDocs) is authored, generated, and verified for this repository.

Scope
- Applies to the whole multi-module project; primary user-facing docs live under the plugin module:
  - Site config: micronaut-maven-plugin/src/site/site.xml
  - AsciiDoc sources: micronaut-maven-plugin/src/site/asciidoc (index.adoc, release-history.adoc, examples/*.adoc)
  - Plugin reports (goals, parameters, etc.) are generated via Maven plugin report tooling and appear in the site under the plugin module.

Tooling and versions (managed in parent POM)
- Maven Site Plugin: org.apache.maven.plugins:maven-site-plugin:3.21.0
- Maven Javadoc Plugin: org.apache.maven.plugins:maven-javadoc-plugin:3.11.3
- Maven Plugin Report Plugin: org.apache.maven.plugins:maven-plugin-report-plugin:${maven-plugin-tools.version}
- Asciidoctor Doxia Converter: org.asciidoctor:asciidoctor-converter-doxia-module:3.2.0
- Doxia Site Renderer: org.apache.maven.doxia:doxia-site-renderer:2.0.0
- Site skin: org.apache.maven.skins:maven-fluido-skin:2.0.0-M11

Source layout and navigation
- Site descriptor: micronaut-maven-plugin/src/site/site.xml
  - Custom menu entries:
    - Overview: index.html, plugin-info.html, release-history.html
    - Examples: examples/*.html (mapped 1:1 from AsciiDoc in src/site/asciidoc/examples/*.adoc)
  - Reports menu is included via <menu ref="reports" /> and will list configured reports (plugin info, project info, checkstyle, javadocs, etc.).
- AsciiDoc authoring:
  - Top-level pages: index.adoc, release-history.adoc
  - Examples directory: src/site/asciidoc/examples/*.adoc
  - Site is rendered with Asciidoctor via Doxia; code highlighting uses coderay with icons=font (configured in parent POM).
  - When adding new example pages:
    1) Create examples/your-topic.adoc
    2) Add a corresponding entry in site.xml under the “Examples” menu
    3) Prefer concise titles and relative links; avoid hardcoding version numbers when possible

Generating the site
- Fast, module-scoped build (recommended when iterating):
  - Generates the site for the plugin module and its dependencies, skipping Invoker tests for speed.
  - Commands:
    ```
    mvn -V -ntp -Dinvoker.skip=true -pl micronaut-maven-plugin -am site
    open micronaut-maven-plugin/target/site/index.html
    ```
- Full project site (all modules):
  - Commands:
    ```
    mvn -V -ntp site
    ```
- Live preview (built-in HTTP server):
  - Useful for local preview without reopening files from target/site.
  - Commands:
    ```
    mvn -V -ntp -pl micronaut-maven-plugin site:run
    ```
- CI/release integration:
  - The release process runs site generation as part of preparation goals:
    - maven-release-plugin: preparationGoals = clean verify site:site
  - Ensure the site builds cleanly locally before performing a release.

JavaDocs
- Configuration (parent POM -> reporting section):
  - maven-javadoc-plugin with:
    - linksource=true
    - minmemory=128m
    - maxmemory=512
    - failOnError=false (site report generation is lenient to avoid breaking site for warnings)
- Javadoc JARs for releases:
  - In profile “release”, the javadoc JAR is attached (goal jar). This must succeed for a release.
  - Quick validation:
    ```
    mvn -V -ntp -Prelease -DskipTests javadoc:jar
    ```
    or scoped to the plugin module:
    ```
    mvn -V -ntp -Prelease -DskipTests -pl micronaut-maven-plugin -am javadoc:jar
    ```
- Authoring guidelines:
  - Public mojos and user-facing classes must include JavaDoc with @since and @author
  - Keep JavaDoc warnings minimal; although site reporting sets failOnError=false, the javadoc:jar during release must pass
  - Use clear, concise JavaDoc. For mojos, summarize the goal and behavior; parameter docs come from plugin annotations but JavaDoc should provide additional context if valuable

Reports included in the site
- Plugin reports:
  - maven-plugin-report-plugin generates plugin-info.html and goal/parameter documentation automatically (linked as “Goals” in site menu)
- Project information reports:
  - maven-project-info-reports-plugin (CI, dependencies, dependency-info, issue-management, licenses, plugin-management, plugins, scm, summary, team)
- Checkstyle report:
  - maven-checkstyle-plugin provides a “Checkstyle” report
- JavaDoc report:
  - maven-javadoc-plugin adds API docs into the site (typically under apidocs/)

Output locations
- Per-module site output: <module>/target/site/
  - For the plugin module: micronaut-maven-plugin/target/site/index.html
- Aggregated javadocs are not explicitly configured; per-module JavaDocs are produced in site by default. Use maven-javadoc-plugin:aggregate only if/when aggregation is needed.

Do’s and Don’ts
- Do:
  - Update AsciiDoc pages under micronaut-maven-plugin/src/site/asciidoc for user-facing changes
  - Keep site.xml in sync with AsciiDoc page additions/removals
  - Run a site build locally before opening PRs that change docs
  - Ensure JavaDoc passes with -Prelease javadoc:jar before tagging a release
- Don’t:
  - Hardcode transient version numbers into docs unless strictly necessary
  - Modify the skin or renderer versions ad-hoc; manage via parent POM properties/plugins

Quick command reference
- Generate site (fast local): mvn -V -ntp -Dinvoker.skip=true -pl micronaut-maven-plugin -am site
- Full site: mvn -V -ntp site
- Live preview: mvn -V -ntp -pl micronaut-maven-plugin site:run
- Validate JavaDoc JAR (release): mvn -V -ntp -Prelease -DskipTests javadoc:jar

PR checklist additions (docs)
- If user-facing behavior changes, update:
  - micronaut-maven-plugin/src/site/asciidoc/*
  - site menu (site.xml) if you add/remove pages
  - README if necessary
- Verify:
  - mvn -V -ntp -Dinvoker.skip=true -pl micronaut-maven-plugin -am site
  - mvn -V -ntp -Prelease -DskipTests javadoc:jar
