var PATHS = [
  "/404.html",
  "/archive.html",
  "/posts/2019-03-13-bank-kata-in-purescript.html",
  "/posts/2019-03-18-testing-bank-kata-in-purescript.html",
  "/posts/2019-03-25-functors-compose-monads-do-not.html",
  "/posts/2019-04-01-a-useless-compiler-in-purescript.html",
  "/posts/2019-04-15-adding-twitter-cards-to-hakyll-posts.html",
  "/posts/2019-05-20-hangman-in-purescript.html",
  "/posts/2019-05-27-nim-in-purescript.html",
  "/posts/2019-06-03-game-of-life.html",
  "/posts/2019-06-10-folding-trees-in-purescript.html",
  "/posts/2019-06-17-a-state-monad-in-purescript.html",
  "/posts/2019-06-24-a-parser-combinator-in-purescript-part-1.html",
  "/posts/2019-07-01-a-parser-combinator-in-purescript-part-2.html",
  "/posts/2019-07-08-scripting-in-haskell-and-purescript.html",
  "/posts/2019-07-15-building-a-blog-in-haskell-with-yesod–the-basic-structure.html",
  "/posts/2019-07-22-building-a-blog-in-haskell-with-yesod–using-a-database.html",
  "/posts/2019-07-29-building-a-blog-in-haskell-with-yesod–authentication.html",
  "/posts/2019-08-05-building-a-blog-in-haskell-with-yesod–authorization.html",
  "/posts/2019-08-12-building-a-blog-in-haskell-with-yesod–returning-JSON.html",
  "/posts/2019-08-19-building-a-blog-in-haskell-with-yesod–returning-JSON-API.html",
  "/posts/2019-08-26-building-a-blog-in-haskell-with-yesod–giving-back.html",
  "/posts/2019-09-02-merging-io-and-either-into-one-monad.html",
  "/posts/2019-09-09-fun-with-typeclasses.html",
  "/posts/2019-09-16-mars-rover-kata-in-haskell.html",
  "/posts/2019-09-23-refactoring-the-mars-rover-kata-in-haskell.html",
  "/posts/2019-09-30-parsing-csv-in-haskell.html",
  "/posts/2019-10-07-playing-with-fmt.html",
  "/posts/2019-12-26-scaffolding-a-blog-post.html",
  "/posts/2020-01-06-posting-a-tweet-with-haskell.html",
  "/posts/2020-01-13-crossposting-via-command-line.html",
  "/posts/2020-01-20-kanbanery-to-trello.html",
  "/posts/2020-02-03-monad-composes-sequentially.html",
  "/posts/2020-02-18-dollar-functor-applicative-monad.html",
  "/posts/2020-02-26-rewriting-haskell-intro.html",
  "/posts/2020-03-03-rewriting-haskell-setup.html",
  "/posts/2020-03-14-rewriting-haskell-server.html",
  "/posts/2020-03-19-rewriting-haskell-formatting.html",
  "/posts/2020-03-23-rewriting-haskell-configuration.html",
  "/posts/2020-03-30-rails.html",
  "/posts/2020-04-06-ghc-options.html",
  "/posts/2020-04-13-rewriting-haskell-testing.html",
  "/posts/2020-04-20-rewriting-haskell-linting.html",
  "/posts/2020-04-23-learning-commercial-projects.html",
  "/posts/2020-04-27-rewriting-haskell-query-params.html",
  "/posts/2020-04-29-boyscout-rule.html",
  "/posts/2020-05-04-rewriting-haskell-query-params-again.html",
  "/posts/2020-05-08-on-productivity.html",
  "/posts/2020-05-11-rewriting-haskell-errors.html",
  "/posts/2020-05-15-living-together-team.html",
  "/posts/2020-05-18-published-posts-hakyll.html",
  "/posts/2020-05-22-bug-vs-mistake.html",
  "/posts/2020-05-25-hakyll-production-drafts.html",
  "/posts/2020-05-29-silly-questions.html",
  "/posts/2020-06-01-records-haskell.html",
  "/posts/2020-06-05-asking-why-to-uncover-assumptions.html",
  "/posts/2020-06-08-custom-markdown-pandoc.html",
  "/posts/2020-06-12-measuring-disagreement-with-standard-deviation.html",
  "/posts/2020-06-15-crossposting-to-medium-via-command-line.html",
  "/posts/2020-06-19-starting-from-the-problem-not-the-solution.html",
  "/posts/2020-06-22-homebrew-brewfile-dump-with-haskell.html",
  "/posts/2020-06-26-why-good-solutions-block-better-ones.html",
  "/posts/2020-06-29-decomposing-features-into-pipelines.html",
  "/posts/2020-07-03-the-secret-to-getting-unstuck-when-investigating-a-bug.html",
  "/posts/2020-07-06-elm-tricks-from-production–intro.html",
  "/posts/2020-07-10-grateful-for-the-opportunity-of-working-on-legacy-code.html",
  "/posts/2020-07-13-elm-tricks-from-production–migrating-from-angular-v1-to-elm.html",
  "/posts/2020-07-17-can-i-be-completely-honest-a-short-mentoring-story.html",
  "/posts/2020-07-20-elm-tricks-from-production–declarative-bug-free-user-interfaces-with-custom-types.html",
  "/posts/2020-07-24-naming-things-made-easy.html",
  "/posts/2020-07-27-elm-tricks-from-production–adding-event-listeners-to-dom-nodes-that-do-not-yet-exist.html",
  "/posts/2020-08-03-elm-tricks-from-production–automated-testing-is-just-another-tool.html",
  "/posts/2020-08-07-the-three-step-recipe-to-success-with-legacy-code-without-getting-overwhelmed.html",
  "/posts/2020-08-10-elm-tricks-from-production–from-angular-v1-to-elm-in-4-days.html",
  "/posts/2020-08-14-how-to-conjure-your-team-magic-with-a-few-stickies-and-the-playbook-exercise.html",
  "/posts/2020-08-20-code-quality-is-free-if-you-do-it-right.html",
  "/posts/2020-08-28-how-to-tame-complexity-into-simplicity-with-a-shake-list.html",
  "/posts/2020-09-02-how-to-tame-your-reading-list-to-support-your-goals.html",
  "/posts/2020-09-10-99-percent-done.html",
  "/posts/2020-09-16-how-to-investigate-performance-issues-in-a-web-app-with-a-simple-script.html",
  "/posts/2020-09-23-making-an-endpoint-13-times-faster.html",
  "/posts/2020-10-01-from-temporary-knowledge-to-permanent-knowledge.html",
  "/posts/2020-10-09-10-knowledge-transfers-that-make-me-thrive-with-legacy-code.html",
  "/posts/2020-10-15-from-zero-to-rxjs-via-knowledge-transfer.html",
  "/posts/2020-10-22-how-to-deal-with-your-unsatisfying-code.html",
  "/posts/2020-10-29-the-simplest-most-powerful-trick-verification-steps.html",
  "/posts/2020-11-05-vanilla-javascript-vs-rxjs.html",
  "/posts/2020-11-11-how-to-terminate-legacy-code-without-getting-stuck.html",
  "/posts/2020-11-19-make-your-tests-fail-randomly-and-profit.html",
  "/posts/2020-11-26-how-would-i-do-it-in-haskell.html",
  "/posts/2020-12-02-advent-of-code-2020.html",
  "/posts/2020-12-21-scripting-the-hell-out-of-trello-in-haskell.html",
  "/posts/2022-08-15-select-and-group-by-in-sql.html",
  "/tags/Applicative.html",
  "/tags/Elm.html",
  "/tags/Essential+Skills.html",
  "/tags/Functional+Programming.html",
  "/tags/Functor.html",
  "/tags/Hakyll.html",
  "/tags/Haskell.html",
  "/tags/Monad.html",
  "/tags/PureScript.html",
  "/tags/Script.html",
  "/tags/Servant.html",
  "/tags/Team+Work.html",
  "/tags/Yesod.html",
  "/tir.html",
];

function handler(event) {
  var request = event.request;
  var uri = request.uri;
  var host = request.headers.host.value;

  if (host.startsWith('www.')) {
    return {
      statusCode: 302,
      statusDescription: 'Found',
      headers: {
        location: {
          value: `https://odone.io${uri}`
        }
      }
    };
  }



  var path = PATHS.find((path) => uri.endsWith(path));

  if (path) {
    return {
      statusCode: 302,
      statusDescription: "Found",
      headers: {
        location: {
          value: path.replace(".html", "/"),
        },
      },
    };
  }

  if (uri.match(/\/newsletter(.*)$/)) {
    return {
      statusCode: 302,
      statusDescription: "Found",
      headers: {
        location: {
          value: "/#newsletter",
        },
      },
    };
  }

  if (uri.match(/\/tips(.*)$/)) {
    return {
      statusCode: 302,
      statusDescription: "Found",
      headers: {
        location: {
          value: "https://ko-fi.com/odone",
        },
      },
    };
  }

  if (uri.endsWith('/')) {
      request.uri += 'index.html';
  } else if (!uri.includes('.')) {
      request.uri += '/index.html';
  }

  return request;
}
