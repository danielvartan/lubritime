# Contributing to `lubritime`

<!-- This CONTRIBUTING.md was adapted from https://gist.github.com/peterdesmet/e90a1b0dc17af6c12daf6e8b2f044e7c -->

First of all, thanks for considering contributing to `lubritime`! 👍 It's people like you that make it rewarding for us---the project maintainers---to work on `lubritime`. 😊

`lubritime` is an open source project, maintained by people who care. We are not directly funded to do so.

[repo]: https://github.com/danielvartan/lubritime/
[issues]: https://github.com/danielvartan/lubritime/issues
[new_issue]: https://github.com/danielvartan/lubritime/issues/new
[website]: https://danielvartan.github.io/lubritime/
[citation]: https://danielvartan.github.io/lubritime/authors.html

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](https://www.contributor-covenant.org/version/2/1/code_of_conduct/). By participating in this project you agree to abide by its terms.

## How You Can Contribute

There are several ways you can contribute to this project. If you want to know more about why and how to contribute to open source projects like this one, see this [Open Source Guide](https://opensource.guide/how-to-contribute/).

### Share the Love ❤️

Think `lubritime` is useful? Help others discover it by sharing in person, on social media, or through a blog post. Don't forget to **star** the [GitHub repository][repo]!

Using `lubritime` for a paper you are writing? Consider [citing it][citation].

### Propose an Idea 💡

Have an idea for a new `lubritime` feature? Take a look at the [documentation][website] and [issue list][issues] to see if it isn't included or suggested yet. If not, suggest your idea as an [issue on GitHub][issues]. While we can't promise to implement your idea, it helps to:

- Explain in detail how it would work
- Keep the scope as narrow as possible

See below if you want to contribute code for your idea as well.

### Report a Bug 🐛

Using `lubritime` and discovered a bug? That's annoying! Don't let others have the same experience and report it as an [issue on GitHub][new_issue] so we can fix it. A good bug report makes it easier for us to do so, so please include:

- The content of `utils::sessionInfo()` (Tip: use the [reprex](https://reprex.tidyverse.org/) package)
- Any details about your local setup that might be helpful in troubleshooting
- Detailed steps to reproduce the bug

### Improve the Documentation 📖

Noticed a typo on the website? Think a function could use a better example? Good documentation makes all the difference, so your help to improve it is very welcome!

#### The Website

[This website][website] is generated with [`pkgdown`](http://pkgdown.r-lib.org/). That means we don't have to write any html: Content is pulled together from documentation in the code, vignettes, [Markdown](https://guides.github.com/features/mastering-markdown/) files, the package `DESCRIPTION` and `_pkgdown.yml` settings. If you know your way around `pkgdown`, you can [propose a file change](https://help.github.com/articles/editing-files-in-another-user-s-repository/) to improve documentation.

#### Function Documentation

Functions are described as comments near their code and translated to documentation using [`roxygen2`](https://roxygen2.r-lib.org/). If you want to improve a function description:

1. Go to the `R` directory in the [code repository][repo]
2. Look for the file with the function
3. [Propose a file change](https://help.github.com/articles/editing-files-in-another-user-s-repository/) to update the function documentation in the roxygen comments (starting with `#'`)

### Contribute Code 📝

Care to fix bugs or implement new functionality for `lubritime`? Awesome! 👏 Have a look at the [issue list][issues] and leave a comment on the things you want to work on. See also the development guidelines below.

## Development Guidelines

We try to follow the [GitHub flow](https://guides.github.com/introduction/flow/) for development. Here are the steps to contribute:

1. Fork [this repository][repo] and clone it to your computer. To learn more about this process, see [this guide](https://guides.github.com/activities/forking/).
2. If you have forked and cloned the project before and it has been a while since you worked on it, [pull changes from the original repo](https://help.github.com/articles/merging-an-upstream-repository-into-your-fork/) to your clone by using `git pull upstream main`
3. Open the project in RStudio or other [IDE](https://en.wikipedia.org/wiki/Integrated_development_environment) of your choice
4. Make your changes:
    - Write your code
    - Test your code (Bonus points for adding unit tests)
    - Document your code (See function documentation above)
    - Check your code with `devtools::check()` and aim for 0 errors, warnings and notes
5. Commit and push your changes
6. Submit a [pull request](https://guides.github.com/activities/forking/#making-a-pull-request)

Also note that this project use [Tidyverse design principles](https://design.tidyverse.org/) and the [Tidyverse style guide](https://style.tidyverse.org/). Your code must conform to this principles and rules.
