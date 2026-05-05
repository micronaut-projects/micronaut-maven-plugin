# Micronaut Maven Plugin maintenance tasks

## Triage incoming issues

New issues need to be categorized with at least one of the following labels:

* `type: bug`: when something is not working as designed.
* `type: improvement`: a minor improvement over an existing feature.
* `type: enhancement`: a completely new feature.
* `type: docs`: documentation change.

Labels that are useful for changelog generation include:

* `type: breaking`.
* `type: deprecated`.
* `type: removed`.

Issues with these labels show up in their own changelog sections.

Sometimes, before accepting bugs, we need to ask for more information from the requester or validate that the report is
actually a bug. These labels help with that workflow:

* `status: awaiting feedback`: waiting for more information from the user.
* `status: awaiting validation`: maintainers need to validate that it is actually an issue.
* `status: awaiting third-party`: the issue is blocked by a bug in a third-party library.

When blockers are cleared, remove the awaiting labels manually. Related status labels include:

* `status: validated`: the issue is ready to be worked on.
* `status: acknowledged`: the issue is understood but not necessarily scheduled.
* `status: in progress`: the issue is currently being worked on.

If we are unsure whether we want or can solve an issue, use:

* `status: under consideration`: the issue is being considered but has not been accepted yet.
* `status: future consideration`: the issue will not be fixed now but can be revisited later.
* `status: next major version`: the issue needs a breaking change and therefore belongs in the next major version.

Use `relates-to` labels to categorize issues by area. The majority of labels are defined in the
[management](https://github.com/micronaut-projects/management/blob/master/labels.tf) repo and propagated via Terraform.
If you need new labels:

* If they are useful across several repositories, send a pull request to the management repo.
* If they are repository-specific, create them in the GitHub UI.

Issues, especially bugs, should be prioritized with `priority: high`, `priority: medium`, or `priority: low`. See the
[Issue Priority Labels](https://github.com/micronaut-projects/micronaut-core/wiki/Issue-Priority-Labels) document for
guidelines.

## Review pull requests

Pull requests, regardless of whether they are created by internal or external contributors, should meet these criteria:

* All GitHub checks are passing, including CLA and build checks.
* Code quality is appropriate for Micronaut projects and uses Micronaut APIs correctly.
* Tests are included for changed behavior.
* Documentation is updated when behavior, configuration, defaults, migration paths, or examples change.
* If the PR closes any issues, they should be linked with closing keywords or manually.

Regarding the target branch, backwards-compatible bug fixes and improvements typically target the default branch,
backwards-compatible enhancements target the next minor version branch, and breaking changes target the next major version
branch. Check the
[Micronaut Module Versioning](https://github.com/micronaut-projects/micronaut-core/wiki/Micronaut-Module-Versioning)
document for more information.

Before merging pull requests, ensure they target the correct branch so breaking changes do not leak into a patch or minor
release. Check the
[Micronaut Module Branch Naming](https://github.com/micronaut-projects/micronaut-core/wiki/Micronaut-Module-Branch-Naming)
document for more information.

## Files sync

The [micronaut-project-template](https://github.com/micronaut-projects/micronaut-project-template) repository is the
source of truth for common repository files. This Maven plugin repository keeps common governance and metadata files as
close as practical while preserving Maven-specific build, CI, wrapper, release, and style behavior.

Files that are generally safe to keep aligned include:

* Issue templates under `.github/ISSUE_TEMPLATE`.
* `.editorconfig`, `.clineignore`, `SECURITY.md`, `MAINTAINING.md`, and `LICENSE`.
* Generic license and formatting metadata where it does not conflict with Maven-specific checks.

Files that require Maven-specific review rather than direct copying include:

* GitHub Actions workflows and scripts, especially release, snapshot, wrapper, and vulnerability-audit automation.
* Wrapper files. This repository uses Maven wrapper files under `.mvn/`, `mvnw`, and `mvnw.cmd`, not the template's
  Gradle wrapper files.
* Checkstyle and Spotless configuration, because this repository's Maven build enforces these in compile/verify phases.
* Renovate rules, because this repository contains Maven-specific dependency and Dockerfile exceptions.

If a template change touches CI expectations, update `.github/workflows/snapshot.yml`, `.github/workflows/windows-ci.yml`,
and/or `.github/workflows/release.yml` consistently, then run `bash .github/scripts/ci-sensitive-preflight.sh`.

## Releases

Releases are performed from GitHub releases and Maven release automation. Publish the draft release with a `v`-prefixed
tag such as `v5.0.0`, then monitor the `Release` workflow.

The release workflow prepares the Maven release, publishes the generated site to GitHub Pages, and performs the Maven
release. Do not replace it with the Gradle-oriented template release flow.

If there is an issue with the release, do not publish artifacts to Maven Central again for the same version. Maven Central
artifacts are immutable once released.
