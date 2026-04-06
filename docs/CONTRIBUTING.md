# Contributing to supabaseR

Thank you for your interest in contributing to supabaseR!

## Getting Started

1.  Fork the repository
2.  Clone your fork locally
3.  Create a new branch following our branch naming conventions (see
    below)
4.  Make your changes and test them
5.  Commit using conventional commits (see below)
6.  Submit a Pull Request

## Commit Convention

We use [Conventional Commits](https://www.conventionalcommits.org/) for
all commit messages.

Examples:

    feat(db): add support for JSON columns
    fix(query): resolve WHERE clause escaping issue
    docs: update installation instructions

## Branch Naming

Branch names follow the same convention as commits:

    <type>/<short-description>

Examples: `feat/add-storage-support`, `fix/connection-timeout`,
`docs/update-readme`

## Development

``` r
# Install dependencies
install.packages("devtools")

# Load package
devtools::load_all()

# Run tests
devtools::test()

# Build documentation
devtools::document()
```

## Guidelines

- Keep things readable and follow existing conventions of code structure
- Add roxygen2 documentation for all exported functions
- Include tests for new features and ensure all tests pass before
  submitting
- Update NEWS.md with your changes

Thank you for contributing!
