# Elm

&nbsp;

## [Shaping Values with Types](https://thoughtbot.com/blog/shaping-values-with-types)
by [Josh Clayton](https://twitter.com/joshuaclayton)

While there are only 110,000 valid four- and five-digit employee IDs, our data model for an employee ID uses the underlying type of String, which can represent an infinite number of values. Our data model does not reflect reality. By reducing the number of possible values captured in a type, it’s less likely that an incorrect value sneaks in.

&nbsp;

## [Running Out of Maps](https://thoughtbot.com/blog/running-out-of-maps)
by [Joël Quenneville](https://twitter.com/joelquen)

Many Elm packages provide map2, map3, map4, etc functions. No matter how many of these the package author has provided, inevitably someone will end up needing a mapN larger than those included in the package. Perhaps that “someone” is you. How do you deal with a situation where you run out of maps?

&nbsp;

## [Tail-Call Elimination](https://functional-programming-in-elm.netlify.app/recursion/tail-call-elimination.html)
by [Evan Czaplicki](https://twitter.com/evancz)

First, a common technique for writing recursive functions is to create a function foo with the API you want and a helper function fooHelp that has a couple additional arguments for accumulating some result. This definitely can come in handy if you are working on some more advanced algorithms!

Second, it turns out that “Is recursion going to crash my program?” is not a practical concern in functional languages like Elm. With linear structures (like List) we can do tricks to take advantage of tail-call elimination. With branching structures (like Dict, Set, and Array) you are likely to run out of disk space before you run out of stack space. Those are the only two kinds of structures we have!

&nbsp;

## [Performant Elm](https://juliu.is/performant-elm/)
by [Ju Liu](https://twitter.com/arkh4m)

You have written your first application in Elm. Congratulations my friend, the hard work has finally paid off. Now it’s time to relax and enjoy the pure bliss that is maintaining and refactoring Elm code.

But wait a second, someone tells you the app feels a bit sluggish to use. Mh. I thought that writing pure functions would automatically make code fast. Okay. Let’s try to stay calm.

&nbsp;

## [elm-graphql](https://elm-radio.com/episode/elm-graphql)
by [Elm Radio Podcast](https://twitter.com/elmradiopodcast)

&nbsp;

## [Elm Radio: Parse, Don't Validate](https://elm-radio.com/episode/parse-dont-validate)
by [Elm Radio Podcast](https://twitter.com/elmradiopodcast)

&nbsp;

## [Making Impossible States Impossible](https://www.youtube.com/watch?v=IcgmSRJHu_8)
by [Richard Feldman](https://twitter.com/rtfeldman)

Among the most time-consuming bugs to track down are the ones where we look at our application state and say "this shouldn’t be possible."

We can use Elm’s compiler to rule out many of these bugs in the first place—but only if we design our Models using the right techniques! This talk explores how.

&nbsp;

## [Opaque Types Let You Think Locally](https://incrementalelm.com/opaque-types-let-you-think-locally/)
by [Dillon Kearns](https://twitter.com/dillontkearns)

Elm's Opaque Types are a powerful tool for narrowing the surface area where you check a constraint. TypeScript's Branded Types give similar functionality but without closing outside use, so you can't be sure the constraints are enforced everywhere. 

&nbsp;

## [Make Data Structures](https://www.youtube.com/watch?v=x1FU3e0sT1I)
by [Richard Feldman](https://twitter.com/rtfeldman)

Start by thinking about what data structures I'm going to use to represent my application.

# Haskell / PureScript

&nbsp;

## [Elementary programming](https://www.michaelpj.com/blog/2021/01/02/elementary-programming.html)
by [Michael Peyton Jones](https://twitter.com/mpeytonjones/)

So we have a dilemma: abstraction is great for the advanced users, but is inevitably going to make your code hard for less advanced users to understand. And if they can’t understand your code, they certainly aren’t going to be able to maintain it.

&nbsp;

## [Getting acquainted with Lens](https://www.youtube.com/watch?v=LBiFYbQMAXc)
by [Paweł Szulc](https://twitter.com/EncodePanda)

&nbsp;

## [Programming Without Type Classes](https://www.youtube.com/watch?v=QDleESXlZJw)
by [Berlin Functional Programming Group](https://www.youtube.com/channel/UCNp-DVb8cQRIOo32sZhWgNg)

Type classes have become a cornerstone of statically-typed functional programming, powering abstractions like monoid and monad. Yet, type classes often have generalized names, which don’t reflect their purpose in specific domains; and they incur higher learning costs, especially when emulated in languages without them.

&nbsp;

## [The Misunderstood Roots of FRP Can Save Programming](https://futureofcoding.org/essays/dctp.html)
by [Steve Krouse](https://twitter.com/stevekrouse)

For many years I been searching for the perfect paradigm for programming user interfaces. Like many others, I fell in love with FRP with the rise of ReactJS and spent a few years searching for the perfect reactive model. Eventually, I found my way back to the original work on FRP by Conal Elliott. It took me almost a year to make sense of it. 

This essay attempts to make Conal’s vision more understandable to less mathematically-oriented programmers, and also show how this perspective could be the foundation for a new era of programming, not just with user interfaces, but also multi-node computing, storage, machine learning, etc.

&nbsp;

## [Anamorphisms aka Unfolds Explained](https://www.works-hub.com/learn/number-anamorphisms-aka-unfolds-explained-50e1a)
by [Marty Stumpf](https://www.linkedin.com/in/thealmarty/)

Unfolds can be thought of as the dual of folds. As Conal Elliott puts it: while folds contract a structure down to a value, unfolds expand a structure up from a value!

&nbsp;

## [Denotational Design: From Meanings To Programs](https://www.youtube.com/watch?v=bmKYiUOEo2A)
by [Conal Elliot](https://twitter.com/conal)

I'll share a methodology that I have applied many times over the last 20+ years when designing high-level libraries for functional programming. Functional libraries are usually organized around small collections of domain-specific data types together with operations for forming and combining values of those types. When done well, the result has the elegance and precision of algebra on numbers while capturing much larger and more interesting ideas.

&nbsp;

## [Bind The Gap](https://bindthegap.news)
by [Kowainik](https://twitter.com/kowainik)

Bind The Gap is a monthly magazine about Haskell and Functional Programming in general.

It discusses hot news in the Haskell ecosystem, describes recent events, educates about FP concepts, and provides a view on diverse language areas from experienced Haskell users and active community members.

&nbsp;

## [(((Wait a moment .) .) .) - Composing Functions with Multiple Arguments](https://ubikium.gitlab.io/portfolio/wait-a-moment.html)
by [Ubik](https://ubikium.gitlab.io)

The goal of the point-free style is the same as all other forms of abstraction: to express your intention in a concise (to write), clear (to read), and general (to change) way.
&nbsp;

## [Composing predicates](https://dev.to/gillchristian/composing-predicates-30jb)
by [Christian Gill](https://twitter.com/gillchristian)

Note that by compose I mean not function composition but to combine two predicates together to get a new one. Either by conjunction (&&) or disjunction (||).

&nbsp;

## [PureScript by Example](https://book.purescript.org/index.html)
by [Phil Freeman](https://twitter.com/paf31)

PureScript is a small strongly, statically typed programming language with expressive types, written in and inspired by Haskell, and compiling to Javascript.

Functional programming in JavaScript has seen quite a lot of popularity recently, but large-scale application development is hindered by the lack of a disciplined environment in which to write code. PureScript aims to solve that problem by bringing the power of strongly-typed functional programming to the world of JavaScript development.

This book will show you how to get started with the PureScript programming language, from the basics (setting up a development environment) to the advanced.

&nbsp;

## [Tying Shoes with GADTs](https://www.morrowm.com/posts/2021-08-02-shoes.html)
by [MorrowM](https://www.morrowm.com)

Do you put them both on and then tie them both? In what order? Do you put on one shoe, tie it and then do the other one? That might be a bit odd, but it’s acceptable. Anyone who ties either of their shoes before putting them on is a no-go, though. And since my Haskell file is my world, and GHC is my enforcer, let’s make this state of affairs unrepresentable.

&nbsp;

## [Simpler And Safer API Design Using GADTs](https://chrispenner.ca/posts/gadt-design)
by [Chris Penner](https://twitter.com/chrislpenner)

A lot of the writing out there regarding GADTs is pretty high-level research and academia, in contrast, today I'm going to show off a relatively practical and simple use-case. In this post we'll take a look at a very real example where we can leveraged GADTs in a real-world Haskell library to build a simple and expressive end-user interface.

&nbsp;

## [Free Course On Functional Programming in Haskell](https://www.i-programmer.info/news/150-training-a-education/14437-free-course-on-functional-programming-in-haskell-.html)
by [Nikos Vaggalis](https://www.linkedin.com/in/nikos-vaggalis/)

Videos from an introductory course by Professor Graham Hutton from the University of Nottingham have been made freely available on YouTube. Designed for first year Computer Science students, they teach the basic principles of functional programming using Haskell.

&nbsp;

## [Refactoring Recursion](https://www.youtube.com/watch?v=F5NjINDNwYc)
by [Harold Carr](https://twitter.com/haroldcarr)

Recursion is the fundamental looping mechanism in functional programming. This talk shows patterns of recursion using Haskell. It shows those patterns for list structure only. This makes it easier for beginners to understand recursion schemes by focusing on their operation with lists. We start by writing explicit recursive versions of sum, product, and length of lists, then factor them into fold functions. We proceed in a similar manner with other folds, unfolds, and refolds with many examples of the patterns in operation. We end by mentioning factoring recursion out of data.

&nbsp;

## [Zippers—Learn You a Haskell for Great Good!](http://learnyouahaskell.com/zippers)
by [Miran Lipovaca](https://www.oreilly.com/pub/au/5027)

One thing we can do is to remember a path from the root of the tree to the element that we want to change. We could say, take this tree, go left, go right and then left again and change the element that's there. While this works, it can be inefficient. If we want to later change an element that's near the element that we previously changed, we have to walk all the way from the root of the tree to our element again!

In this chapter, we'll see how we can take some data structure and focus on a part of it in a way that makes changing its elements easy and walking around it efficient. Nice!

&nbsp;

## [Time travel in Haskell for dummies](https://kcsongor.github.io/time-travel-in-haskell-for-dummies/)
by [Csongor Kiss](https://twitter.com/lowert)

Browsing Hackage the other day, I came across the Tardis Monad. Reading its description, it turns out that the Tardis monad is capable of sending state back in time. Yep. Back in time.

&nbsp;

## [Names are not type safety](https://lexi-lambda.github.io/blog/2020/11/01/names-are-not-type-safety/)
by [Alexis King](https://twitter.com/lexi_lambda)

The Haskell school of program construction advocates “capturing invariants in the type system” and “making illegal states unrepresentable,” both of which sound like compelling goals, but are rather vague on the techniques used to achieve them.

&nbsp;

## [Parse, don’t validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
by [Alexis King](https://twitter.com/lexi_lambda)

Now I have a single, snappy slogan that encapsulates what type-driven design means to me, and better yet, it’s only three words long: Parse, don’t validate.

&nbsp;

## [Types as axioms, or: playing god with static types](https://lexi-lambda.github.io/blog/2020/08/13/types-as-axioms-or-playing-god-with-static-types/)
by [Alexis King](https://twitter.com/lexi_lambda)

The real point of this blog post is that a type system does not have a well-defined list of things it “can prove” and “can’t prove.” Languages like TypeScript don’t really encourage this approach to data modeling, where you restructure your values in a certain way so as to guarantee certain properties. Rather, they prefer to add increasingly sophisticated constraints and type system features that can capture the properties people want to capture without having to change their data representation.

&nbsp;

## [Haskell for the Elm Enthusiast](https://blog.noredink.com/post/658510851000713216/haskell-for-the-elm-enthusiast)
by [NoRedInk](https://twitter.com/noredink)

Rails has served us well and has supported amazing growth of our website, both in terms of the features it supports, and the number of students and teachers who use it. But we’ve come to miss some of the tools that make us so productive in Elm: Tools like custom types for modeling data, or the type checker and its helpful error messages, or the ease of writing (fast) tests.

A couple of years ago we started looking into Haskell as an alternative backend language that could bring to our backend some of the benefits we experience writing Elm in the frontend. Today some key parts of our backend code are written in Haskell. Over the years we’ve developed our style of writing Haskell, which can be described as very Elm-like (it’s also still changing!).

&nbsp;

## [Parser Combinators: a Walkthrough](https://hasura.io/blog/parser-combinators-walkthrough/)
by [Antoine Leblanc](https://twitter.com/nicuveo)

Today, I want to explore Parsec, and most specifically how Parsec works. Parsing is ubiquitous, and most Haskell programs will use Parsec or one of its variants (megaparsec or attoparsec). While it’s possible to use those libraries without caring about how they work, I think it’s fascinating to develop a mental model for their inner design; namely, monadic parser combinators, as it is a simple and elegant technique that is useful beyond the use case of parsing raw text.

&nbsp;

## [Generalizing 'Jq' And Traversal Systems Using Optics And Standard Monads](https://chrispenner.ca/posts/traversal-systems)
by [Chris Penner](https://twitter.com/chrislpenner)

Hi folks! Today I'll be chatting about Traversal Systems like jq and XPath; we're going to discover which properties make them useful, then see how we can replicate their most useful behaviours in Haskell.

&nbsp;

## [JSON Parsing from Scratch in Haskell: Error Reporting](https://abhinavsarkar.net/posts/json-parsing-from-scratch-in-haskell-2/)
by [Abhinav Sarkar](https://twitter.com/abhin4v)

In the previous post we wrote a simple but correct JSON parser in Haskell. The parser was written very naively: if it failed, it returned nothing. You couldn’t tell what the failure was or where it happened. That’s OK for a toy parser but error reporting is an absolute must requirement for all good parsers. So in this post and next post, we’ll add simple but useful error reporting capability to our JSON parser. 

&nbsp;

## [An introduction to typeclass metaprogramming](https://lexi-lambda.github.io/blog/2021/03/25/an-introduction-to-typeclass-metaprogramming)
by [Alexis King](https://twitter.com/lexi_lambda)

Typeclass metaprogramming is a powerful technique available to Haskell programmers to automatically generate term-level code from static type information. It has been used to great effect in several popular Haskell libraries (such as the servant ecosystem), and it is the core mechanism used to implement generic programming via GHC generics. Despite this, remarkably little material exists that explains the technique, relegating it to folk knowledge known only to advanced Haskell programmers.

# JavaScript / TypeScript

&nbsp;

## [Statically Prevent 404s](https://www.youtube.com/watch?v=KRMJIiGE0ds)
by [Gary Bernhardt](https://twitter.com/garybernhardt)

Most web applications have route patterns like "/courses/:courseId". Then they link to the routes by manually building path strings. But there's no guarantee that links actually point to valid routes. When we change or delete routes, we might forget to update some of the links, turning them into 404s.

&nbsp;

## [PrinceJS](https://princejs.com/)
by [Oliver Klemenz](https://twitter.com/oklemenz)

Prince of Persia [reimplementation](https://github.com/oklemenz/PrinceJS) written in HTML5 / JavaScript (MS-DOS version).

&nbsp;

## [Hasty](https://hasty.dev)
by [Mads B. Cordes](https://twitter.com/Mobilpadde)

JavaScript snippet performance comparison tool

&nbsp;

## [Beyond Console.log() – Level up Your Debugging Skills](https://www.sitepoint.com/beyond-console-log-level-up-your-debugging-skills/)
by [Chris Heilmann](https://twitter.com/codepo8)

You may have established a pattern of coding that utilizes a few key tools offered by your browser’s console. But have you dug any deeper lately? There are some powerful tools available to you, and they might just revolutionize the way you work.

&nbsp;

## [How To Build Resilient JavaScript UIs](https://www.smashingmagazine.com/2021/08/build-resilient-javascript-ui/)
by [Callum Hart](https://twitter.com/_callumhart)

Embracing the fragility of the web empowers us to build UIs capable of adapting to the functionality they can offer, whilst still providing value to users. This article explores how graceful degradation, defensive coding, observability, and a healthy attitude towards failures better equips us before, during, and after an error occurs.

&nbsp;

## [Dynamic Static Typing In TypeScript](https://www.smashingmagazine.com/2021/01/dynamic-static-typing-typescript/)
by [Stefan Baumgartner](https://twitter.com/ddprrt)

In this article, we look at some of the more advanced features of TypeScript, like union types, conditional types, template literal types, and generics. We want to formalize the most dynamic JavaScript behavior in a way that we can catch most bugs before they happen.

&nbsp;

## [Strict null checking Visual Studio Code](https://code.visualstudio.com/blogs/2019/05/23/strict-null)
by [Matt Bierner](https://twitter.com/mattbierner)

In this post, I'd like to share a major engineering effort that the VS Code team recently completed: enabling TypeScript's strict null checking in our codebase. We believe this work will allow us to both move faster and to ship a more stable product. Enabling strict null checking was motivated by understanding bugs not as isolated events but as symptoms of larger hazards in our source code.

Using strict null checking as a case study, I'm going to discuss what motivated our work, how we came up with an incremental approach to addressing the problem, and how we went about implementing the fix. This same general approach to identifying and reducing hazards can be applied to any software project.

&nbsp;

## [The TypeScript Handbook](https://www.typescriptlang.org/assets/typescript-handbook.pdf)
by Microsoft

The TypeScript Handbook is intended to be a comprehensive document that explains TypeScript to everyday programmers. You can read the handbook by going from top to bottom in the left-hand navigation.

&nbsp;

## [End-to-End TypeScript: Database, Backend, API, and Frontend](https://www.youtube.com/watch?v=GrnBXhsr0ng)
by [Gary Bernhardt](https://twitter.com/garybernhardt)

This is a walk-through of a real commercial system written in TypeScript. Static types are used to ensure that the backend code uses the database's data correctly, that the backend sends the correct data over the API, and that the frontend consumes API data correctly.

&nbsp;

## [Optional Chaining: The ?. Operator in TypeScript](https://mariusschulz.com/blog/optional-chaining-the-operator-in-typescript)
by [Marius Schulz](https://twitter.com/mariusschulz)

In this post, I will go over the following three optional chaining operators and explain why we might want to use them in our TypeScript or JavaScript code.

&nbsp;

## [3 Useful TypeScript Patterns to Keep in Your Back Pocket](https://spin.atomicobject.com/2021/05/11/3-useful-typescript-patterns/)
by [Casey Falkowski](https://www.linkedin.com/in/caseyfalkowski/)

Using TypeScript with its most basic types alone adds plenty of safety to your code. Sometimes, however, you come across situations where you need a little bit more. Here are three useful TypeScript patterns that will kick your game up a notch.

&nbsp;

## [10 bad TypeScript habits to break this year](https://startup-cto.net/10-bad-typescript-habits-to-break-this-year/)
by [Daniel Bartholomae](https://twitter.com/the_startup_cto)

TypeScript and JavaScript have steadily evolved over the last years, and some of the habits we built over the last decades have become obsolete. Some might never have been meaningful. Here's a list of 10 habits that we all should break.

&nbsp;

## [Getting started with fp-ts](https://dev.to/gcanti/getting-started-with-fp-ts-setoid-39f3)
by [Giulio Canti](https://twitter.com/GiulioCanti)

In this blog series I will often talk about "type classes" and "instances", let's see what they are and how they are encoded in fp-ts.

&nbsp;

## [Practical Guide to Fp‑ts](https://rlee.dev/practical-guide-to-fp-ts-part-1)
by [Ryan Lee](https://twitter.com/ryanleecode)

This post is an introduction to fp-ts, a functional programming library for Typescript. Why should you be learning fp-ts? The first reason is better type safety. Fp‑ts allows you to make assertions about your data structures without writing user-defined type guards or using the as operator. The second reason is expressiveness and readability. Fp-ts gives you the tools necessary to elegantly model a sequence of operations that can fail. All in all, you should add fp-ts to your repertoire of tools because it will help you write better Typescript programs.

&nbsp;

## [TypeScript Tips](https://typescript.tips/)
by [Riccardo Odone](https://twitter.com/RiccardoOdone)

Make Bugs Impossible. One TypeScript Tip At A Time.

&nbsp;

## [The TypeScript converging point](https://fettblog.eu/slides/the-typescript-converging-point/)
by [Stefan Baumgartner](https://twitter.com/ddprrt)

What we got is type safety for programs that live by using a flexible, string-based API. We transformed string types to strong types. All with just a couple lines of code and some of the more advanced features of TypeScript.

&nbsp;

## [How the TypeScript Compiler Compiles](https://www.youtube.com/watch?v=X8k_4tZ16qU)
by [Orta Therox](https://twitter.com/orta)

A systems-level look at the TypeScript compiler. How it converts a file into something into data, checks the validity of that data and finally creates .js files on the disk. 

&nbsp;

## [TypeScript / How some utility types are implemented](https://www.huy.rocks/everyday/03-30-2022-typescript-how-some-utility-types-are-implemented)
by [Huy](https://twitter.com/huytd189)

TypeScript provides several utility types◹ to help us manipulate types easier, like: Partial<T>, Required<T>, Pick<T, Keys>,…

&nbsp;

## [Opaque Types](https://blog.beraliv.dev/2021-05-07-opaque-type-in-typescript)
by [Alexey Berezin](https://twitter.com/beraliv)

TypeScript, (un)like Elm and Haskell, has a structural type system.

&nbsp;

## [Get the best of TypeScript Control Flow Analysis](https://retool.com/blog/typescript-control-flow-analysis-best-of/)
by [Charly Poly](https://twitter.com/whereischarly)

This article will show some simple type construction patterns, code writing habits, and compiler options that you can use to improve your application's type inference without increasing its complexity.

&nbsp;

## [A Complete Guide To TypeScript’s Never Type](https://www.zhenghao.io/posts/ts-never)
by [Zhenghao He](https://twitter.com/he_zhenghao)

TypeScript’s never type is very under-discussed, because it’s not nearly as ubiquitous or inescapable as other types. A TypeScript beginner can probably ignore never type as it only appears when dealing with advanced types, such as conditional types, or reading their cryptic type error messages.

&nbsp;

## [Gary Bernhardt - TypeScript and Testing](https://fullstackradio.com/144)
by [Full Stack Radio](https://twitter.com/adamwathan)

In this episode, Adam talks to Gary Bernhardt about building Execute Program, why he chose to build it as a full-stack TypeScript application, and the implications using TypeScript has on what you need to test.

# React / Next / Remix

&nbsp;

## [A Look at React Hooks](https://lo-victoria.com/series/a-look-at-react-hooks)
by [Victoria Lo](https://twitter.com/lo_victoria2666)

In this series, we shall look at various React Hooks and learn how we can use them to implement in our React projects!

&nbsp;

## [Useful React Hooks That You Can Use In Your Projects](https://www.smashingmagazine.com/2021/11/useful-react-hooks/)
by [Ifeanyi Dike](https://ifeanyidesmonddike.web.app)

We’ll go through several code examples of each hook and also explore how you’d create a custom hook.

&nbsp;

## [Tao of React - Software Design, Architecture & Best Practices](https://alexkondov.com/tao-of-react/)
by [Alex Kondov](https://twitter.com/alexanderkondov)

While there are best practices on the micro level, most teams build their own “thing” when it comes to architecture.

&nbsp;

## [Guideline from the 70's on how to split your React components](https://joaoforja.com/blog/guideline-on-how-to-decompose-a-react-component/)
by [João Forja](https://twitter.com/ImForja)

Deciding how to break a component into sub-components isn’t easy and is a challenge that isn’t specific to React. This is fortunate since it means we can go outside React’s ecosystem and get some ideas on how to do it.

In this article, I’ll present a guideline to validate ideas on splitting a React component to increase code reuse and reduce maintenance costs. This guideline comes from the paper “Designing Software for Ease of Extension and Contraction” written in 1979 by David Parnas.

&nbsp;

## [The Algebraic Structure of Functions, Illustrated Using React Components](https://jrsinclair.com/articles/2020/algebraic-structure-of-functions-illustrated-with-react-components/)
by [James Sinclair](https://twitter.com/jrsinclair)

Let me be clear. I’m not suggesting anyone go and write all their React components using compose, map, and chain. I’m not even suggesting anyone include a Func library in their codebase. What I am hoping is that this gives you some tools to think differently about your React code. I’m also hoping that the algebraic structure of functions makes a little more sense now.

&nbsp;

## [Algebraic effects, Fibers, Coroutines Oh my!](https://www.youtube.com/watch?v=7GcrT0SBSnI)
by [Brandon Dail](https://twitter.com/aweary)

React Fiber was a full re-write of React that will enable new and exciting patterns around control flow, which we've seen previewed with React Suspense. 

But what is a fiber? How does it relate to a coroutine? What are algebraic effects, and why do I keep hearing about them? 

This talk will go over these computer science topics in the context of React Fiber, to help shed some light on how React Fiber is implemented and the control flow concepts behind the new APIs.

&nbsp;

## [The Story of Next.js](https://www.youtube.com/watch?v=BILxV_vrZO0)
by uidotdev

This is the story of how Next.js took over the world.

&nbsp;

## [Remix](https://remix.run)

Remix is a full stack web framework that lets you focus on the user interface and work back through web fundamentals to deliver a fast, slick, and resilient user experience. People are gonna love using your stuff.

&nbsp;

## [Building my first Remix app!](https://www.youtube.com/watch?v=RwVrEQRI1HU)
by [Sam Selikoff](https://twitter.com/samselikoff)

Follow along as I build a simple tracking app for my workouts using Remix! We end up with a form to create new entries and a homepage to view them, and the whole thing gets deployed to Fly.io.

&nbsp;

## [Data Flow in Remix](https://remix.run/blog/remix-data-flow)
by [Jim Nielsen](https://twitter.com/jimniels)

With Remix, your UI becomes a function of state across the network, not just locally.

&nbsp;

## [RemixConf 2022 - Videos](https://www.youtube.com/playlist?list=PLXoynULbYuEC36XutMMWEuTu9uuh171wx)
by Remix

Remix is a web framework with a laser focus on web fundamentals and modern user experience. Remix Conf is intended to feature members of the Remix community and give them an opportunity to inspire each other to build excellent apps with Remix. #RemixConf 2022 was the first year #RemixConf was held.

\includepdf[pages=1]{intermission.pdf}

&nbsp;

## [Why you should lift component state up to the URL](https://www.youtube.com/watch?v=sFTGEs2WXQ4)
by [Sam Selikoff](https://twitter.com/samselikoff)

"How can I sync component state with the URL?" This is one of the most common questions I see asked in the React community, and in this video we're going to learn why trying to synchronize state between your React app and the URL is a bad idea – and the right way to address this issue.

# Ruby

&nbsp;

## [How to Make a Gem of a Gem](https://blog.testdouble.com/talks/2021-11-10-how-to-make-a-gem-of-a-gem/)
by [Justin Searls](https://twitter.com/searls)

This presentation from RubyConf 2021 will show you every single step involved in creating and releasing a brand new gem—all in the first 8 minutes! The other 22 minutes will distill a decade of hard-fought lessons about how not to make a gem to help you ensure your next gem is a great one.

&nbsp;

## [Hotwire - HTML Over The Wire](https://hotwire.dev)
by Basecamp

Hotwire is an alternative approach to building modern web applications without using much JavaScript by sending HTML instead of JSON over the wire. This makes for fast first-load pages, keeps template rendering on the server, and allows for a simpler, more productive development experience in any programming language, without sacrificing any of the speed or responsiveness associated with a traditional single-page application.

&nbsp;

## [Building HEY with Hotwire](https://fullstackradio.com/151)
by [Full Stack Radio](https://twitter.com/fullstackradio)

In this episode Adam talks to DHH about using Hotwire to develop Basecamp's new email service HEY while shipping only 40kb of JavaScript to the client.

&nbsp;

## [What is a reduction and why Fibers are the answer for Ruby concurrency](http://live.julik.nl/2021/02/why-reductions-are-important)
by [Julik Live](https://twitter.com/julikt)

In my view, for Ruby web apps to be viable concurrency is vastly more beneficial than parallelism. Ractors are, at this stage, problematic with most existing Ruby programs. The fiber scheduling system added to Ruby 3.x is going to be extremely important to the story of Ruby on the server (and of UIs, if that ever reemerges).

&nbsp;

## [Surgical Refactors](https://blog.testdouble.com/talks/2016-09-16-surgical-refactors-with-suture/)
by [Justin Searls](https://twitter.com/searls)

Is there anything we can do to make legacy Ruby more maintainable?

That question led me to this talk. In it, I introduce a new gem that we designed to help wrangle legacy refactors. It’s called Suture, and along with providing some interesting functionality to make refactoring less mysterious and scary, it also prescribes a clear, careful, and repeatable workflow for increasing our confidence when changing legacy code.

&nbsp;

## [The Turbo Rails Tutorial](https://www.hotrails.dev)
by [Alexandre Ruban](https://twitter.com/alexandre_ruban)

Learn how to create modern, single-page, reactive web applications with Ruby on Rails 7 without writing any custom JavaScript code.

# Rust

&nbsp;

## [Type-Driven API Design in Rust](https://www.youtube.com/watch?v=bnnacleqg6k)
by [Will Crichton](https://twitter.com/wcrichton)

In this talk, I will live-code the design of a simple Rust API. Through the evolution of the API, I will demonstrate how Rust’s type system (especially traits) can be used to design interfaces that cleanly compose with existing code, and that help API clients catch mistakes at compile-time.

&nbsp;

## [Take your first steps with Rust](https://docs.microsoft.com/en-us/learn/paths/rust-first-steps/)
by Microsoft

Interested in learning a new programming language that's growing in use and popularity? Start here! Lay the foundation of knowledge you need to build fast and effective programs in Rust.

In this learning path, you'll:

- Install the tools you need to write your first lines of Rust code.
- Learn basic concepts in Rust.
- Learn how to handle errors.
- Manage memory in Rust.
- Use generic types and traits.
- Set up modules for packages and crates.
- Write and run automated tests.
- Create a command-line program.

&nbsp;

## [The Importance of Not Over-Optimizing in Rust](https://www.youtube.com/watch?v=CV5CjUlcqsw)
by [Lily Mara](https://twitter.com/thelily_mara)

In this talk I will show that these optimizations are often unnecessary to get performance that beats highly dynamic languages like Python. For new Rust developers, breaking the temptation to over-optimize can lead to higher productivity and satisfaction with Rust.

# Other Programming Languages

&nbsp;

## [Tackling Concurrency Bugs with TLA+](https://www.youtube.com/watch?v=_9B__0S21y8)
by [Hillel Wayne](https://twitter.com/hillelogram)

Concurrency is hard. How do you test your system when it's spread across three services and four languages? Unit testing and type systems only take us so far. At some point we need new tools.

Enter TLA+. TLA+ is a specification language that describes your system and the properties you want. This makes it a fantastic complement to testing: not only can you check your code, you can check your design, too! TLA+ is especially effective for testing concurrency problems, like stalling, race conditions, and dropped messages.

&nbsp;

## [Darklang - Demo: Office Sign-in Application](https://www.youtube.com/watch?v=orRn2kTtRXQ)
by [Ellen Chisa](https://twitter.com/ellenchisa)

A demo of using Dark to build an office sign in application (API, data storage, external API connections and background workers).

&nbsp;

## [The Unison Programming Language with Rúnar Bjarnason](https://mobile.twitter.com/sw_unscripted/status/1517588731150188546)
by [Richard Feldman](https://twitter.com/rtfeldman)

Richard talks with Unison Programming Language co-creator Rúnar Bjarnason about the things that make Unison unlike any other programming language in the world.

&nbsp;

## [A (Not So Gentle) Introduction To Systems Programming In ATS](https://www.youtube.com/watch?v=zt0OQb1DBko)
by [Aditya Siram](https://twitter.com/deech)

The recent surge of interest in secure memory management has sparked a renaissance of type safe systems programming languages. ATS is a statically typed ML that compiles to C and offers what almost no other modern systems language including Rust has: type safe pointer arithmetic.

&nbsp;

## [Lua in 100 Seconds](https://www.youtube.com/watch?v=jUuqBZwwkQw)
by Fireship

Lua is a lightweight dynamic scripting language often embedded into other programs like World of Warcraft and Roblox. It's minimal syntax makes it easier to learn than Python, while being much more performant than other interpreted languages. 

&nbsp;

## [Generate web forms from pure functions](https://www.haskellforall.com/2022/05/generate-web-forms-from-pure-functions.html)
by [Gabriella Gonzalez](https://twitter.com/GabriellaG439)

This project is a web page which can dynamically convert a wide variety of functional programming expressions to their equivalent HTML. This conversion can even auto-generate interactive web forms from functions, which means that people without web programming knowledge can use the Grace browser to create and share simple and interactive web pages.

# Sql / NoSql

&nbsp;

## [Why is it hard to automatically suggest what index to create?](https://www.depesz.com/2021/10/22/why-is-it-hard-to-automatically-suggest-what-index-to-create/)
by [Depesz](https://www.linkedin.com/in/depesz/)

Should I put it in index? Or not? What will I do, if it has 20% selectivity, but together with condition on d it will drop to 2%? Should I index it on (a,d)? (d,a)? (a) where d = ‘done'?

&nbsp;

## [Some Indexing Best Practices](https://www.pgmustard.com/blog/indexing-best-practices-postgresql)
by [Michael Christofides](https://twitter.com/michristofides)

We all know that indexing is important, but it can be difficult to know where to start. In this post, my aim is to collate some of the best advice I’ve seen for indexing Postgres, and simplify it for folks just getting started. 

&nbsp;

## [Exploring PL/pgSQL: Strings, arrays, recursion, and parsing JSON](https://notes.eatonphil.com/exploring-plpgsql.html)
by [Phil Eaton](https://twitter.com/phil_eaton)

PostgreSQL comes with a builtin imperative programming language called PL/pgSQL. I used to think this language was scary because it has a bit more adornment than your usual language does. But looking deeper, it's actually reasonably pleasant to program in.

&nbsp;

## [Building Robust Systems with ACID and Constraints](https://brandur.org/acid)
by [Brandur](https://twitter.com/brandur)

In the last decade we’ve seen the emergence of a number of new flavors of data store that come with untraditional features like streaming changesets, JavaScript APIs, or nestable JSON documents. Most of them assume that the need for horizontal partitioning is a given in this day and age and therefore ACID is put to the altar (this doesn’t necessarily have to be the case, see below). Every decision comes with trade-offs, but trading away these powerful guarantees for the novelties du jour or an unexamined assumption that horizontal scaling will very soon be a critically required feature is as raw of a deal as you’ll ever see in the technical world.

&nbsp;

## [Using checksums to verify syncing 100M database records](https://sirupsen.com/napkin/problem-14-using-checksums-to-verify/)
by [Simon Eskildsen](https://twitter.com/Sirupsen)

In this issue of napkin math, we look at implementing a solution to check whether A and B are in sync for 100M records in a few seconds. The key idea is to checksum an indexed updated\_at column and use a binary search to drill down to the mismatching records.

&nbsp;

## [Draw Entity-Relationship Diagrams, Painlessly](https://dbdiagram.io)
by holistics.io

A free, simple tool to draw ER diagrams by just writing code.
Designed for developers and data analysts.

&nbsp;

## [Speeding up SQL queries by orders of magnitude using UNION](https://www.foxhound.systems/blog/sql-performance-with-union/)
by [Ben Levy](https://www.linkedin.com/in/benjamin-levy-1a614678/) and [Christian Charukiewicz](https://www.linkedin.com/in/charukiewicz/)

One of the most common cases where SQL query performance can degrade significantly is in a diamond shaped schema, where there are multiple ways of joining two tables together. In such a schema, a query is likely to use OR to join tables in more than one way, which eliminates the optimizer’s ability to create an efficient query plan.

&nbsp;

## [Advanced SQL course | SQL tutorial advanced](https://www.youtube.com/watch?v=2Fn0WAyZV0E)
by [Carnagie Mellon Database Group](https://db.cs.cmu.edu)

SQL is a domain-specific language used in programming and designed for managing data held in a relational database management system, or for stream processing in a relational data stream management system.
In this advance #SQL​ course you will learn how to execute complex queries. 

&nbsp;

## [Cut Out the Middle Tier: Generating JSON Directly from Postgres](https://blog.crunchydata.com/blog/generating-json-directly-from-postgres)
by [Paul Ramsey](https://twitter.com/pwramsey)

PostgreSQL has built-in JSON generators that can be used to create structured JSON output right in the database, upping performance and radically simplifying web tiers.

&nbsp;

## [PostgreSQL: Detecting Slow Queries Quickly](https://www.cybertec-postgresql.com/en/postgresql-detecting-slow-queries-quickly/)
by [Hans-Jürgen Schönig](https://twitter.com/postgresql_007)

I believe the best and most efficient way to detect performance problems is to make use of pg\_stat\_statement, which is an excellent extension shipped with PostgreSQL and is used to inspect query statistics in general. It helps you to instantly figure out which queries cause bad performance and how often they are executed.

&nbsp;

## [Finding the Root Cause of Slow Postgres Queries Using EXPLAIN](https://resources.pganalyze.com/pganalyze_Finding_the_root_cause_of_slow_Postgres_queries_using_EXPLAIN.pdf)
by pganalyze

This situation might look familiar: Our application is slow, but we don't know why. We look at our monitoring dashboards and finally arrive at the center of our problem:

A Postgres query that is taking longer than it should.

Now where do we go from here? How can we understand better what our database does when it executes our slow query to retrieve our data?

&nbsp;

## [Postgresql: How to write a trigger](https://www.cybertec-postgresql.com/en/postgresql-how-to-write-a-trigger/)
by [Hans-Jürgen Schönig](https://twitter.com/postgresql_007)

Just like in most databases, in PostgreSQL a trigger is a way to automatically respond to events. Maybe you want to run a function if data is inserted into a table. Maybe you want to audit the deletion of data, or simply respond to some UPDATE statement. That is exactly what a trigger is good for. This post is a general introduction to triggers in PostgreSQL. It is meant to be a simple tutorial for people who want to get started programming them.

&nbsp;

## [Forget SQL vs NoSQL - Get the Best of Both Worlds with JSON in PostgreSQL](https://arctype.com/blog/json-in-postgresql/#nosql)
by [Derek Xiao](https://twitter.com/lofiderek)

Have you ever started a project and asked - "should I use a SQL or NoSQL database?"

It’s a big decision. There are multiple horror stories of developers choosing a NoSQL database and later regretting it.

But now you can get the best of both worlds with JSON in PostgreSQL.

In this article I cover the benefits of using JSON, anti-patterns to avoid, and an example of how to use JSON in Postgres.

&nbsp;

## [Using ON Versus WHERE Clauses to Combine and Filter Data in PostgreSQL Joins](https://www.pluralsight.com/guides/using-on-versus-where-clauses-to-combine-and-filter-data-in-postgresql-joins)
by [Jacek Trociński](https://www.jtrocinski.com)

In an SQL query, data can be filtered in the WHERE clause or the ON clause of a join. This guide will examine the difference between the two in PostgreSQL.

&nbsp;

## [A simple example of LATERAL use](https://fluca1978.github.io/2021/08/07/PostgreSQLLateralJoin.html)
by [Luca Ferrari](https://fluca1978.github.io/about/)

A few days ago I found a question by a user on Facebook: how to select events from a table where they are no more than 10 minutes one from another? 
My first answer was related to LATERAL, and this post I try to represent with an example how I understood and could solve the above question. 

&nbsp;

## [Saving a Tree in Postgres Using LTREE](http://patshaughnessy.net/2017/12/13/saving-a-tree-in-postgres-using-ltree)
by [Pat Shaughnessy](https://twitter.com/pat_shaughnessy)

LTREE allows me to save, query on and manipulate trees or hierarchical data structures using a relational database table. As we’ll see, using LTREE I can count leaves, cut off branches, and climb up and down trees easily - all using SQL right inside my application’s existing Postgres database!

&nbsp;

## [Explaining Your Postgres Query Performance](https://blog.crunchydata.com/blog/get-started-with-explain-analyze)
by [Kat Batuigas](https://twitter.com/alsokrista)

In a previous post, I talked about pg\_stat\_statements as a tool for helping direct your query optimization efforts. Now let's say you've identified some queries you want to look into. The EXPLAIN command helps you look even closer into an individual query. If you're already proficient in EXPLAIN, great! Read on for an easy refresher. If you're less familiar with it, this will be a (hopefully) gentle introduction on what insights it might help provide.

&nbsp;

## [Index Advisor for Postgres](https://pganalyze.com/index-advisor)
by pganalyze

The pganalyze Index Advisor analyzes your query, determines how Postgres will scan the tables involved, and may suggest indexes to make those scans more efficient.

&nbsp;

## [How to create (lots!) of sample time-series data with PostgreSQL generate_series()](https://blog.timescale.com/blog/how-to-create-lots-of-sample-time-series-data-with-postgresql-generate_series/)
by [Ryan Booz](https://twitter.com/ryanbooz)

Generating sample time-series data with the PostgreSQL generate\_series() function is a useful skill to have when evaluating new database features, creating demonstrations, or testing insert and query patterns. Learn what PostgreSQL `generate_series()` is and how to use it for basic data generation.

&nbsp;

## [JSON in PostgreSQL: How to do it Right](https://www.cybertec-postgresql.com/en/json-postgresql-how-to-use-it-right/)
by [Laurenz Albe](https://github.com/laurenz)

The comprehensive JSON support in PostgreSQL is one of its best-loved features. Many people – particularly those with a stronger background in Javascript programming than in relational databases – use it extensively. However, my experience is that the vast majority of people don’t use it correctly. That causes problems and unhappiness in the long run.

&nbsp;

## [Postgres Window Magic](https://youtu.be/D8Q4n6YXdpk)
by [Bruce Momjian](https://momjian.us)

&nbsp;

## [Modern data analysis with PostgreSQL – JSONB throws Window functions out the…](https://swarm64.com/post/postgresql-jsonb-vs-window-functions)
by [Thomas Richter](https://www.linkedin.com/in/thomas-d-richter/)

In this blog post I would like to take a look at a classical data layout paradigm and how transactional database features, such as fast UPDATEs and JSON/JSONB types, can make analytics  easier.

&nbsp;

## [Fuzzy Name Matching in Postgres](https://blog.crunchydata.com/blog/fuzzy-name-matching-in-postgresql)
by [Paul Ramsey](https://twitter.com/pwramsey)

A surprisingly common problem in both application development and analysis is: given an input name, find the database record it most likely refers to. It's common because databases of names and people are common, and it's a problem because names are a very irregular identifying token.

&nbsp;

## [The Art Of PostgreSQL](https://www.youtube.com/watch?v=q9IXCdy_mtY)
by [Dimitri Fontaine](https://twitter.com/tapoueh)

Applications nowadays are written with the help of many programming languages. When the backend should implement user oriented workflows, it may rely on a RDBMS component to take care of the system’s integrity.

PostgreSQL is the world’s most advanced open source relational database, and is very good at taking care of your system’s integrity. PostgreSQL also comes with a ton of data processing power, and in many cases a simple enough SQL statement may replace hundreds of lines of code written in Python, Java, PHP, Ruby, Javascript, you name it.

In this talk, we learn advanced SQL techniques and how to reason about which part of the backend code should be done in the database, and which part of the backend code is better written as an SQL query.

&nbsp;

## [Postgres SQL Lessons From Advent of Code Challenges](https://heap.io/blog/lessons-from-completing-a-few-advent-of-code-problems-in-postgres-sql)
by Heap

We did something odd for Advent of Code this year: We solved a few challenges in javascript and then in PostgreSQL. We learned a few interesting things about SQL that we'd like to share here.

&nbsp;

## [Efficient Pagination Using Deferred Joins](https://aaronfrancis.com/2022/efficient-pagination-using-deferred-joins)
by [Aaron Francis](https://twitter.com/aarondfrancis)

Paginating records across large datasets in a web application seems like an easy problem that can actually be pretty tough to scale. The two main pagination strategies are offset/limit and cursors.

We'll first take a look at the two methods and then a slight modification that can make offset/limit extremely performant.

&nbsp;

## [Changing Tires at 100mph: A Guide to Zero Downtime Migrations](https://kiranrao.ca/2022/05/04/zero-downtime-migrations.html)
by Kiran Rao

This guide will go through the step-by-step process of migrating tables in PostgreSQL. While the examples are for a PostgreSQL table migration, the same steps can apply to almost any migration.

&nbsp;

## [SQL tricks and concepts you didn't know about](https://www.youtube.com/watch?v=QoCGmvVCqto)
by [Aleksandra Sikora](https://twitter.com/aleksandrasays)

Did you know that some SQL variants are Turing complete and let you write any program in SQL? Of course, no one's that crazy... But what are the limits of SQL? What are some crazy things we can do with it? I'm going to go over a few of them in this talk. It won't be only fun stuff, though! I'm going to show some more practical but lesser-known concepts too. Let's discover some hidden SQL traits together!

&nbsp;

## [Five Easy to Miss PostgreSQL Query Performance Bottlenecks](https://pawelurbanek.com/postgresql-query-bottleneck)
by [Paweł Urbanek](https://twitter.com/_pawurb)

PostgreSQL query planner is full of surprises, so a common-sense approach to writing performant queries is sometimes misleading. In this blog post, I’ll describe examples of optimizing seemingly obvious queries with the help of EXPLAIN ANALYZE and Postgres metadata analysis.

&nbsp;

## [A Hairy PostgreSQL Incident](https://ardentperf.com/2022/02/10/a-hairy-postgresql-incident/)
by [Jeremy Schneider](https://twitter.com/jer_s)

It was 5:17pm today, just as I was wrapping up work for the day, and my manager pinged me with the following chat:

&nbsp;

## [Automating my job by using GPT-3 to generate database-ready SQL to answer business questions](https://blog.seekwell.io/gpt3)
by [Brian Kane](https://www.linkedin.com/in/brian-kane-214a97ba/)

I want to be able to describe a question in plain English and have GPT-3 convert it into the SQL code that, if executed on my Postgres database, would answer the question. 

&nbsp;

## [AWS Aurora VS DynamoDB](https://www.youtube.com/watch?v=crHwekf0gTA)
by [Daniel](https://twitter.com/BeABetterDevv)

RDS Aurora is a popular SQL based Database Engine offered by AWS. DynamoDB is the competitor, a NoSQL database engine that enables limitless scaling potential with excellent performance. Wondering what the difference is between them and which one is right for your new project? Then this is the video for you. 

&nbsp;

## [Cloud SQLite](https://brandur.org/nanoglyphs/034-cloud-sqlite)
by [Brandur](https://twitter.com/brandur)

This is one of those moments where we might legitimately be looking at a turning point for web technology – like when the combination of MySQL and PHP made it possible for anyone to build a dynamic, stateful application, or when Amazon changed the world with S3 and EC2. We could look back on this moment in ten years and think of monolithic programs like Postgres akin to how we think about Oracle today – hopelessly outdated in our modern world of fully distributed, streaming databases.

# Graphql

&nbsp;

## [GraphQL is a trap](https://twitter.com/jmhodges/status/1522282897905840128)
by [Jeff Hodges](https://twitter.com/jmhodges)

GraphQL makes your public API equal to a generic database and -- worse -- a generic graph database. The amount of maintenance work is sky high. Locking the query capabilities down means you're just doing normal APIs but not locking it down means infinite performance work

&nbsp;

## [GraphQL is a Trap?](https://xuorig.medium.com/graphql-is-a-trap-e83ca380aa8f)
by [Marc-André Giroux](https://twitter.com/__xuorig__)

This [twitter thread](https://twitter.com/jmhodges/status/1522385068974432256) blew up on twitter yesterday and I thought I’d go over some of the author’s points in a longer format so we can clear up some misconceptions. Let’s go over them one by one!


# Test

&nbsp;

## [Magic Test](https://twitter.com/andrewculver/status/1366062684802846721)
by [Andrew Culver](https://twitter.com/andrewculver) and [Adam Pallozzi](https://twitter.com/adampallozzi)

Magic Test allows you to write Rails system tests interactively through a combination of trial-and-error in a debugger session and also just simple clicking around in the application being tested, all without the slowness of constantly restarting the testing environment.

&nbsp;

## [Testing Your Edge Cases](https://thoughtbot.com/blog/testing-your-edge-cases)
by [Joël Quenneville](https://twitter.com/joelquen)

Do we have any tests that cover the nil case?

&nbsp;

## [Domain Invariants & Property-Based Testing for the Masses](https://www.youtube.com/watch?v=pX44CoRSIpg)
by [Romeu Moura](https://twitter.com/malk_zameth)

Domain invariants are all around you. In every business rule your domain expert ever tried to give you. You should use them to guide your design.

You should also be testing them! Not only an example representing them: but testing the invariant itself.

You can do them with property-based tests (PBTs). If you think you cannot do PBTs on legacy codebases (or outside FP) this talk should show you otherwise.

Let’s also use Property-based tests to reduce test-debt. Create smaller, fewer tests that: test more, are more readable & document the problem. Challenge your understanding of the domain and communication with domain experts.

&nbsp;

## [Prefer Fakes Over Mocks](https://tyrrrz.me/blog/fakes-over-mocks)
by [Tyrrrz](https://twitter.com/Tyrrrz)

In this article we will look at the differences between these two variants of test doubles, identify how using one over the other impacts test design, and why using fakes often results in more manageable test suites.

&nbsp;

## [Cross-Branch Testing](https://www.hillelwayne.com/post/cross-branch-testing/)
by [Hillel Wayne](https://twitter.com/hillelogram)

There’s a certain class of problems that’s hard to test:

1. The output isn’t obviously inferable from the input. The code isn’t just solving a human-tractable problem really quickly, it’s doing something where we don’t know the answer without running the program.
2. The output doesn’t have “mathematical” properties, like invertibility or commutativity.
3. Errors in the function can be “subtle”: there can be a bug that affects only a small subset of possible inputs, so that a set of individual test examples would still be correct.

&nbsp;

## [Boundaries](https://www.youtube.com/watch?v=yTkzNHF6rMs)
by [Gary Bernhardt](https://twitter.com/garybernhardt)

Some people test in isolation, mocking everything except the class under test. We'll start with that idea, quickly examine the drawbacks, and ask how we might fix them without losing the benefits. This will send us on a trip through behavior vs. data, mutation vs. immutability, interface vs. data dependencies, how data shape affords parallelism, and what a system optimizing each of these for natural isolation might look like.

# Performance

&nbsp;

## [Why Averages Suck and Percentiles are Great](https://www.dynatrace.com/news/blog/why-averages-suck-and-percentiles-are-great/)
by [Michael Kopp](https://twitter.com/mikopp)

Anyone that ever monitored or analyzed an application uses or has used averages. They are simple to understand and calculate. We tend to ignore just how wrong the picture is that averages paint of the world.

&nbsp;

## [Speed is the killer feature](https://bdickason.com/posts/speed-is-the-killer-feature/)
by [Brad Dickason](https://twitter.com/bdickason)

Speed is a killer feature. Speed is a differentiator.

Yet teams consistently overlook speed. Instead, they add more features (which ironically make things slower). Products bloat over time and performance goes downhill.

New features might help your users accomplish something extra in your product. Latency stops your users from doing the job they already hire your product for.

&nbsp;

## [Loading Third-Party JavaScript](https://developers.google.com/web/fundamentals/performance/optimizing-content-efficiency/loading-third-party-javascript)
by [Addy Osmani](https://twitter.com/addyosmani) and [Arthur Evans](https://twitter.com/devdocdude)

You've optimized all of your code, but your site still loads too slowly. Who's the culprit?

Often, performance problems slowing pages down are due to third-party scripts: ads, analytics, trackers, social-media buttons, and so on.

Third-party scripts provide a wide range of useful functionality, making the web more dynamic, interactive, and interconnected. These scripts may be crucial to your website's functionality or revenue stream. But third-party scripts also come with many risks that should be taken into consideration to minimize their impact while still providing value.

&nbsp;

## [Reflections on software performance](https://blog.nelhage.com/post/reflections-on-performance/)
by [Nelson Elhage](https://twitter.com/nelhage)

I’ve really come to appreciate that performance isn’t just some property of a tool independent from its functionality or its feature set. Performance — in particular, being notably fast — is a feature in and of its own right, which fundamentally alters how a tool is used and perceived.

# DevOps

&nbsp;

## [Write your first workflow with GitHub Actions and GitHub APIs](https://www.youtube.com/watch?v=-hVG9z0fCac)
by [Bassem Dghaidi](https://twitter.com/bassemdy)

We will cover how the internals of GitHub Actions, workflow syntax, jobs, steps, functions, conditionals, community actions, log viewer, and I will be showing you step by step how you can use GitHub Actions to call GitHub's REST APIs to automate certain tasks!

&nbsp;

## [The Unfulfilled Promise of Serverless](https://www.lastweekinaws.com/blog/the-unfulfilled-promise-of-serverless/)
by [Corey Quinn](https://twitter.com/QuinnyPig)

I suggest that serverless computing, or “serverless” has hype that at this point has outpaced what the technology / philosophy / religion has been promising. 

&nbsp;

## [The Unfulfilled Potential of Serverless](https://www.jeremydaly.com/the-unfulfilled-potential-of-serverless/)
by [Jeremy Daly](https://twitter.com/jeremy_daly)

[...] we’ve barely scratched the surface of what’s possible with serverless technology.

&nbsp;

## [Forget a server — bring your static website to life with AWS S3, Lambda, and API Gateway](https://medium.com/wearejullio/forget-a-server-bring-your-static-website-to-life-with-aws-s3-lambda-and-api-gateway-8734baaada6f)
by Jull.io

An entire backend server for something as simple as a contact form seems quite overkill, and, well — it is.

&nbsp;

## [Why We Need More Chaos - Chaos Engineering, That Is](https://www.youtube.com/watch?v=rgfww8tLM0A)
by [Nora Jones](https://twitter.com/nora_js)

[Nora] discusses the benefits and challenges of establishing Chaos Engineering into your systems.

&nbsp;

## [Failing over without falling over](https://stackoverflow.blog/2020/10/23/adrian-cockcroft-aws-failover-chaos-engineering-fault-tolerance-distaster-recovery/)
by [Adrian Cockcroft](https://twitter.com/adrianco)

You’ve gone through the motions and play-acted a disaster recovery scenario, but despite spending a lot on the production, it’s not real. What you have is a fairy tale: “Once upon a time, in theory, if everything works perfectly, we have a plan to survive the disasters we thought of in advance.” In practice, it’s more likely to be a nightmare.

&nbsp;

## [The NGINX Handbook](https://www.freecodecamp.org/news/the-nginx-handbook/)
by [Farhan Hasin Chowdhury](https://twitter.com/frhnhsin)

After going through the entire book, you should be able to:

- Understand configuration files generated by popular tools as well as those found in various documentation.
- Configure NGINX as a web server, a reverse proxy server, and a load balancer from scratch.
- Optimize NGINX to get maximum performance out of your server.

&nbsp;

## [Amazon Web Services In Plain English](https://expeditedsecurity.com/aws-in-plain-english/)
by Expedited Security

Hey, have you heard of the new AWS services: ContainerCache, ElastiCast and QR72? Of course not, I just made those up.

But with 50 plus opaquely named services, we decided that enough was enough and that some plain english descriptions were needed.

&nbsp;

## [Introduction to Terraform - A Practical Approach](https://www.youtube.com/watch?v=H0EQR3LGRz0)
by [Matthew Sanabria](https://twitter.com/sudomateo)

This video introduces Terraform through a practical approach by walking through how a fictional company would use Terraform to manage their DigitalOcean infrastructure.

\includepdf[pages=1]{intermission.pdf}

# Legacy Code / Maintenance

&nbsp;

## [The Path of Madness](https://brandur.org/nanoglyphs/029-path-of-madness)
by [Brandur](https://twitter.com/brandur)

The above is a non-exaggerated description of the life of a programmer in Oracle fixing a bug. Now imagine what horror it is going to be to develop a new feature. It takes 6 months to a year (sometimes two years!) to develop a single small feature.

&nbsp;

## [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
by [Olivier Lacan](https://twitter.com/olivierlacan) and Tyler Fortune

Who needs a changelog?

People do. Whether consumers or developers, the end users of software are human beings who care about what's in the software. When the software changes, people want to know why and how.

&nbsp;

## [When costs are nonlinear, keep it small](https://jessitron.com/2021/01/18/when-costs-are-nonlinear-keep-it-small/)
by [Jessica Kerr](https://twitter.com/jessitron)

Batching work is more efficient … until cost rises nonlinearly with batch size. Then smaller batches are the most efficient. So don’t delay maintenance!

&nbsp;

## [Managing technical quality in a codebase](https://lethain.com/managing-technical-quality/)
by [Will Larson](https://twitter.com/Lethain)

If there's one thing that engineers, engineering managers, and technology executives are likely to agree on, it's that there's a crisis of technical quality. One diagnosis and cure is easy to identify: our engineers aren't prioritizing quality, and we need to hire better engineers or retrain the ones we have. Of course, you should feel free to replace "engineers" with "Product Managers" or "executives" if that feels more comfortable. It's a compelling narrative with a clear villain, and it conveniently shifts blame away from engineering leadership. Still, like most narratives that move accountability towards the folks with the least power, it's both unhelpful and wrong.

&nbsp;

## [The Wall of Technical Debt](https://verraes.net/2020/01/wall-of-technical-debt/)
by [Mathias Verraes](https://twitter.com/mathiasverraes)

The Wall of Technical Debt is a surface in your office where you visualize issues on sticky notes. It’s easy to start and maintain, and yet it has a profound impact on how you choose to add, reduce, pay back, or ignore debt. It’s by no means intended as a complete solution to scale the management of debt, but it works precisely because it requires no buy-in.

# Web

&nbsp;

## [How MDN’s autocomplete search works](https://hacks.mozilla.org/2021/08/mdns-autocomplete-search/)
by [Peter Bengtsson](https://twitter.com/peterbe)

Last month, Gregor Weber and I added an autocomplete search to MDN Web Docs, that allows you to quickly jump straight to the document you’re looking for by typing parts of the document title. This is the story about how that’s implemented. If you stick around to the end, I’ll share an “easter egg” feature that, once you’ve learned it, will make you look really cool at dinner parties. Or, perhaps you just want to navigate MDN faster than mere mortals.

&nbsp;

## [Have Single-Page Apps Ruined the Web?](https://www.youtube.com/watch?v=860d8usGC0o)
by [Rich Harris](https://twitter.com/Rich_Harris)

The backlash to modern front end development is gaining steam, with good reason: single-page apps have ruined the web. Can we rescue it without going backwards? In this talk, Rich Harris presents a way to do just that.

&nbsp;

## [JWT should not be your default for sessions](https://evertpot.com/jwt-is-a-bad-default/)
by [Evert Pot](https://twitter.com/evertp)

Using JWTs for tokens add some neat properties and make it possible in some cases for your services to be stateless, which can be desirable property in some architectures.

Adopting them comes with drawbacks. You either forego revocation, or you need to have infrastructure in place that be way more complex than simply adopting a session store and opaque tokens.

My point in all this is not to discourage the use of JWT in general, but be deliberate and careful when you do. Be aware of both the security and functionality trade-offs and pitfalls. Keep it out of your ‘boilerplates’ and templates, and don’t make it the default choice.

&nbsp;

## [The Future of Web Software Is HTML-over-WebSockets](https://alistapart.com/article/the-future-of-web-software-is-html-over-websockets/)
by [Matt E. Patterson](https://twitter.com/mepatterson)

The future of web-based software architectures is already taking form, and this time it’s server-rendered (again). Papa’s got a brand new bag: HTML-over-WebSockets and broadcast everything all the time.

&nbsp;

## [Getting started with WebRTC](https://webrtc.org/getting-started/overview)
by Google

The WebRTC standard covers, on a high level, two different technologies: media capture devices and peer-to-peer connectivity.

Media capture devices includes video cameras and microphones, but also screen capturing "devices". For cameras and microphones, we use navigator.mediaDevices.getUserMedia() to capture MediaStreams. For screen recording, we use navigator.mediaDevices.getDisplayMedia() instead.

The peer-to-peer connectivity is handled by the RTCPeerConnection interface. This is the central point for establishing and controlling the connection between two peers in WebRTC.

&nbsp;

## [Deep dive in CORS: History, how it works, and best practices](https://ieftimov.com/post/deep-dive-cors-history-how-it-works-best-practices/)
by [Ilija Eftimov](https://twitter.com/itsilija)

Learn the history and evolution of same-origin policy and CORS, understand CORS and the different types of cross-origin access in depth, and learn (some) best practices.

&nbsp;

## [The State Of Web Workers In 2021](https://www.smashingmagazine.com/2021/06/web-workers-2021/)
by [Surma](https://twitter.com/dassurma)

The web is single-threaded. This makes it increasingly hard to write smooth and responsive apps. Workers have a bad rep, but can be an important and useful tool in any web developer’s toolbelt for these kinds of problems. Let’s get up to speed on Workers on the Web!

&nbsp;

## [A Look at Server-Sent Events](https://simonprickett.dev/a-look-at-server-sent-events/)
by [Simon Prickett](https://twitter.com/simon_prickett)

Server Sent Events are a standard allowing browser clients to receive a stream of updates from a server over a HTTP connection without resorting to polling. Unlike WebSockets, Server Sent Events are a one way communications channel - events flow from server to client only.

&nbsp;

## [A11y Coffee](https://a11y.coffee)
by [Amberley](https://twitter.com/amber1ey)

Pick your serving size of web accessibility information.

&nbsp;

## [How to use undocumented web APIs](https://jvns.ca/blog/2022/03/10/how-to-use-undocumented-web-apis/)
by [Julia Evans](https://twitter.com/b0rk)

A couple of days I wrote about tiny personal programs, and I mentioned that it can be fun to use “secret” undocumented APIs where you need to copy your cookies out of the browser to get access to them.

&nbsp;

## [We're all living on it. But what exactly is The Edge?](https://whitep4nth3r.com/blog/what-is-the-edge-serverless-functions/)
by [Salma Alam-Naylor](https://twitter.com/whitep4nth3r)

More and more cloud hosting providers and software-as-a-service platforms are offering serverless functions at The Edge. But what is The Edge? What are Edge Functions? And why does it matter?

&nbsp;

## [In Defence of the Single Page Application](https://williamkennedy.ninja/javascript/2022/05/03/in-defence-of-the-single-page-application/)
by [William Kennedy](https://twitter.com/_williamkennedy)

&nbsp;

## [How to design better APIs](https://r.bluethl.net/how-to-design-better-apis)
by [Ronald Blüthl](https://twitter.com/rbluethl)

15 language-agnostic, actionable tips on REST API design.

# Http

&nbsp;

## [HTTPWTF](https://httptoolkit.tech/blog/http-wtf/)
by [Tim Perry](https://twitter.com/pimterry)

HTTP is fundamental to modern development, from frontend to backend to mobile. But like any widespread mature standard, it's got some funky skeletons in the closet.

Some of these skeletons are little-known but genuinely useful features, some of them are legacy oddities relied on by billions of connections daily, and some of them really shouldn't exist at all. Let's look behind the curtain:

&nbsp;

## [The Idempotency-Key HTTP Header Field](https://datatracker.ietf.org/doc/html/draft-ietf-httpapi-idempotency-key-header-00)
by [Sanjay Dalal](https://www.linkedin.com/in/sanjaydalal/) and IETF

The HTTP Idempotency-Key request header field can be used to carry idempotency key in order to make non-idempotent HTTP methods such as "POST" or "PATCH" fault-tolerant.

&nbsp;

## [The Long Road To HTTP/3](https://scorpil.com/post/the-long-road-to-http3/)
by [Andrew Savchyn](https://twitter.com/scorpil)

While HTTP/3 specification is still in the draft stage, the latest version of the Chrome browser already supports it by default . With Chrome holding around 70% of browser market share, you could say HTTP/3 has gone mainstream.

&nbsp;

## [How HTTPS works ...in a comic!](https://howhttps.works)
by Dnsimple

Have you ever wondered why a green lock icon appears on your browser URL bar? And why is it important? We did too, and this comic is for you!

&nbsp;

## [REST / HTTP methods: POST vs. PUT vs. PATCH](https://www.mscharhag.com/api-design/http-post-put-patch)
by [Michael Scharhag](https://twitter.com/mscharhag)

In general we can say:

- POST requests create child resources at a server defined URI. POST is also used as general processing operation
- PUT requests create or replace the resource at the client defined URI
- PATCH requests update parts of the resource at the client defined URI

&nbsp;

## [HTTP methods: Idempotency and Safety](https://www.mscharhag.com/api-design/http-idempotent-safe)
by [Michael Scharhag](https://twitter.com/mscharhag)

Idempotency and safety are properties of HTTP methods. The HTTP RFC defines these properties and tells us which HTTP methods are safe and idempotent. Server application should make sure to implement the safe and idempotent semantic correctly as clients might expect it.

&nbsp;

## [HTTP SEARCH Method](https://datatracker.ietf.org/doc/draft-ietf-httpbis-safe-method-w-body)
by [Julian Reschke](https://twitter.com/jreschke), Ashok Malhotra, [James Snell](https://www.linkedin.com/in/jasnell/)

Many existing HTTP-based applications use the HTTP GET and POST methods in various ways to implement the functionality provided by SEARCH.


# Software in General

&nbsp;

## [No, dynamic type systems are not inherently more open](https://lexi-lambda.github.io/blog/2020/01/19/no-dynamic-type-systems-are-not-inherently-more-open/)
by [Alexis King](https://twitter.com/lexi_lambda)

Internet debates about typing disciplines continue to be plagued by a pervasive myth that dynamic type systems are inherently better at modeling “open world” domains. The argument usually goes like this: the goal of static typing is to pin everything down as much as possible, but in the real world, that just isn’t practical. Real systems should be loosely coupled and worry about data representation as little as possible, so dynamic types lead to a more robust system in the large.


&nbsp;

## [What Is Functional Programming?](http://blog.jenkster.com/2015/12/what-is-functional-programming.html)
by [Kris Jenkins](https://twitter.com/krisajenkins)

This is my take on what functional programming really is, in a way that will make sense to a jobbing programmer just trying to Get Stuff Done.


&nbsp;

## [Finding Service Boundaries](https://vimeo.com/113515335)
by [Udi Dahan](https://twitter.com/UdiDahan)

Although Service-Oriented Architecture is a fairly well-known topic, there are very few good examples out there of the application of SOA principles to non-trivial domains so developers don’t have much to learn from. In this presentation, Udi will show a case study from the healthcare domain resulting in services so autonomous they almost don’t have to share any data with each other – whether that’s through request/response, events, or via a shared data store.

&nbsp;

## [Essays on programming I think about a lot](https://www.benkuhn.net/progessays/)
by [Ben Kuhn](https://twitter.com/benskuhn)

Every so often I read an essay that I end up thinking about, and citing in conversation, over and over again. Here’s my index of all the ones of those I can remember!

&nbsp;

## [Domain Modelling Made Functional](https://www.youtube.com/watch?v=1pSH8kElmM4)
by [Scott Wlaschin](https://twitter.com/ScottWlaschin)

Types can be used to represent the domain in a fine-grained, self documenting way. And in many cases, types can even be used to encode business rules so that you literally cannot create incorrect code. You can then use the static type checking almost as an instant unit test — making sure that your code is correct at compile time.

&nbsp;

## [How we ship code faster and safer with feature flags](https://github.blog/2021-04-27-ship-code-faster-safer-feature-flags/)
by [Alberto Gimeno](https://twitter.com/gimenete)

At GitHub, we’re continually working to improve existing features and shipping new ones all the time. From our launch of GitHub Discussions to the release of manual approvals for GitHub Actions—in order to ship new features and improvements faster while lowering the risk in our deployments, we have a simple but powerful tool: feature flags.

&nbsp;

## [The Wrong Abstraction](https://sandimetz.com/blog/2016/1/20/the-wrong-abstraction)
by [Sandi Metz](https://twitter.com/sandimetz)

The moral of this story? Don't get trapped by the sunk cost fallacy. If you find yourself passing parameters and adding conditional paths through shared code, the abstraction is incorrect. It may have been right to begin with, but that day has passed. Once an abstraction is proved wrong the best strategy is to re-introduce duplication and let it show you what's right. Although it occasionally makes sense to accumulate a few conditionals to gain insight into what's going on, you'll suffer less pain if you abandon the wrong abstraction sooner rather than later.

&nbsp;

## [Minimalism versus Types](https://www.youtube.com/watch?v=VUThGgxOf28)
by [Hisham](https://twitter.com/hisham_hm)

We love minimalistic languages because they let us do so much with so little. But when we start doing a lot with them, often we start yearning for types to help us make sense of it all. Adding types to a minimalistic language (well, adding anything!) makes it larger. Is this worth the price? Is a rich type system antithetical to minimalism? Let's find out!

&nbsp;

## [The data model behind Notion's flexibility](https://www.notion.so/blog/data-model-behind-notion)
by [Jake Teton-Landis](https://twitter.com/jitl)

Everything you see in Notion is a block. Text, images, lists, a row in a database, even pages themselves — these are all blocks, dynamic units of information that can be transformed into other block types or moved freely within Notion

&nbsp;

## [How to deal with money in software](https://cs-syd.eu/posts/2022-08-22-how-to-deal-with-money-in-software)
by [Tom Sydney Kerckhove](https://twitter.com/kerckhove_ts)

Dealing with money in software is difficult and dangerous.

&nbsp;

## [Rethinking Best Practices](https://willgallego.com/2021/08/14/rethinking-best-practices/)
by [Will Gallego](https://twitter.com/wcgallego)

The concepts behind best practices may be sound, but as they are not ubiquitous, they should be up for debate. Our tendency to skip deeper investigation, to assume an answer is correct based on a label, makes use of the term “best practice” dangerous.

&nbsp;

## [Breaking Up the Behemoth](https://sandimetz.com/blog/2017/9/13/breaking-up-the-behemoth)
by [Sandi Metz](https://twitter.com/sandimetz)

Sticking with procedures too long is just as bad as doing design too soon. If important classes in your domain change often, get bigger every time they change, and are accumulating conditionals, stop adding to them right now. Use every new request as an opportunity to do a bit of design. Implement the change using small, well-designed classes that collaborate with the existing object.

&nbsp;

## [Falling Into The Pit of Success](https://blog.codinghorror.com/falling-into-the-pit-of-success/)
by [Jeff Atwood](https://twitter.com/codinghorror)

Wouldn't it be nice to use a language designed to keep you from falling into the pit of despair? But avoiding horrific, trainwreck failure modes isn't a particularly laudable goal. Wouldn't it be even better if you used a language that let you effortlessly fall into The Pit of Success?

&nbsp;

## [Preemptive Pluralization is (Probably) Not Evil](https://www.swyx.io/preemptive-pluralization)
by [Swyx](https://twitter.com/swyx)

Before you write any code — ask if you could ever possibly want multiple kinds of the thing you are coding. If yes, just do it. Now, not later.

&nbsp;

## [Ramblings About Clean Code](https://mailchi.mp/4b5982e14519/rambling-about-clean-code)
by [Alex Kondov](https://twitter.com/alexanderkondov)

I would advise you not to worry that much about whether your code will be considered clean but whether it serves its purpose.

&nbsp;

## [Functional Programming for Pragmatists](https://www.youtube.com/watch?v=3n17wHe5wEw)
by [Richard Feldman](https://twitter.com/rtfeldman)

People like functional programming for different reasons. Some like it for the conceptual elegance, or the mathematical properties. Richard? He likes to build things. He likes it when the software he builds works well and is easy to maintain.

&nbsp;

## [Code Smells - Developing Design Sense for your Code](https://www.youtube.com/watch?v=H3L0aN9KItA)
by [Llewellyn Falco](https://twitter.com/LlewellynFalco)

'In order to make delicious food, you must eat delicious food…. you need to develop a palate capable of discerning good and bad. Without good taste, you can't make good food.' - Jiro Ono (World’s Best Sushi Chef). The same is true for great code. 

&nbsp;

## [José Valim, creator of Elixir and form Rails core contributor](https://remoteruby.transistor.fm/178)
by [Remote Ruby](https://twitter.com/remote_ruby)

Today, our discussions take us through José’s background, being a Rails core member, and the story of how he created Elixir. He also goes in depth about LiveView, distributed systems, how using Elixir and Phoenix is a great developer experience, new and exciting things he’s working on with Elixir, and he fills us in on Nerves, FarmBot, Broadway, and Numerical Elixir. Go ahead and download this episode now to find out more!

&nbsp;

## [Falsehoods programmers believe about time zones](https://www.zainrizvi.io/blog/falsehoods-programmers-believe-about-time-zones/)
by [Zain Rizvi](https://twitter.com/zainrzv)

I soon discovered just how wrong I was. One after another, I kept learning the falsehood of yet another "fact" that had seemed obviously true.

&nbsp;

## [Serverless TACTICAL DD(R)](https://levelup.gitconnected.com/serverless-tactical-dd-r-23d18d529fa1)
by [Lee James Gilmore](https://twitter.com/LeeJamesGilmore)

Serverless TACTICAL DD(R) as a tactical approach to DoR and DoD aims to ensure we ‘consider’ and ‘validate’ key areas which are in my experience typically forgotten about when teams are quickly trying to realise business value when building out their solutions, and unfortunately are rarely back-ported, and typically forgotten about until issues arise!

# Team

&nbsp;

## [Get your work recognized: write a brag document](https://jvns.ca/blog/brag-documents/)
by [Julia Evans](https://twitter.com/b0rk)

There’s this idea that, if you do great work at your job, people will (or should!) automatically recognize that work and reward you for it with promotions / increased pay. In practice, it’s often more complicated than that – some kinds of important work are more visible/memorable than others. It’s frustrating to have done something really important and later realize that you didn’t get rewarded for it just because the people making the decision didn’t understand or remember what you did. So I want to talk about a tactic that I and lots of people I work with have used!

&nbsp;

## [Tidepool Employee Handbook](https://tidepool.atlassian.net/wiki/spaces/HANDBOOK/overview?homepageId=636027069)
by Tidepool

Q: Why can’t we just say “No politics at work?”

A: Our mission is inherently political. Also, one of our values is to “fight the default of exclusion.”

&nbsp;

## [Hyperproductive development](https://jessitron.com/2017/06/24/the-most-productive-circumstances-for/)
by [Jessica Kerr](https://twitter.com/jessitron)

Let’s talk about why some developers, in some situations, are ten times more productive than others.

hint: it isn’t the developers, so much as the situation.

&nbsp;

## [Bourdieu's social theory applied to tech](https://www.youtube.com/watch?v=3P1sRZ56YRg)
by [Romeu Moura](https://twitter.com/malk_zameth)

Obscure though it may seems, the sociologist Bourdieu and his social theory tell us a lot about what is happening in the workplace and society around us. By understanding what he meant by "symbolic violence", "cultural capital" "hexis" etc, we see how each of us influences and is influenced by the people around us, in ways that we wouldn't expect.  From this talk, a vulgarized and easy to understand version of  Bourdieu's ideas, each of us can seek how to improve the ambience immediately around us.

&nbsp;

## [Pushing through Friction](https://www.youtube.com/watch?v=8bxZuzDKoI0)
by [Dan Na](https://www.linkedin.com/in/danielna/)

As a senior-level engineering leader, experience tells you things could be better. You see the gaps. If only the company adopted policy A or dumped technology B, everyone would benefit. But there's so much inertia. The company has always used B. You are frustrated. Can you actually make a difference?

&nbsp;

## [Sociotechnical Lenses into Software Systems](https://paulosman.me/2021/10/02/sociotechnical-lenses-into-software-systems/)
by [Paul Osman](https://twitter.com/paulosman)

The systems we are building and operating are constantly being modified by different people, with different contexts, at different times, who may or may not speak to each other directly. This emergent collaboration can present unique challenges that are fun to navigate.

&nbsp;

## [Meeting Resistance and Moving Forward](https://www.youtube.com/watch?v=DrCD1wmK9oM)
by [Linda Rising](https://twitter.com/RisingLinda)

It's "those skeptical people" who are most annoying. They don't seem to listen to our ideas. They usually start raising objections before we have even finished describing what we are thinking. They have a counterargument for every argument. What's to be done with "those people"? In this presentation, Linda will pull patterns from the Fearless Change collection plus the latest research in neuroscience to help you in the challenges you face with resistance.

&nbsp;

## [Team Norming Ceremony: Build Effective Remote Collaboration from Day One](https://magicbell.com/blog/effective-remote-collaboration)
by [Hana Mohan](https://twitter.com/unamashana)

Tools such as Zoom and Slack have made remote collaboration not just manageable but downright efficient. One challenge still present is the human element of working together. Is a teammate a morning or night person? Do they prefer Slacking asynchronously or Zooming face-to-face? Are there differences in personal values amongst the team?

&nbsp;

## [The best retrospective for beginners](https://retromat.org/blog/best-retrospective-for-beginners/)
by [Corinna Baldauf](https://twitter.com/CorinnaBaldauf)

Are you new to facilitating retrospectives? Then you’re probably wondering how to best get started. For what it’s worth, here’s my “Given that I know nothing about you or the team’s situation here’s my best shot at a multi-purpose, easy to facilitate retrospective plan”.

&nbsp;

## [Manifesto—Build a company you want to work for](https://tighten.com/manifesto/)
by Tighten

We imagined that there might be a better way to run a company, where the primary purpose was to provide a space for people to care for one another, and the actual work output was a crucial but secondary purpose. So we set out to create the company we would want to work for.

# Project Management

&nbsp;

## [The Joy of Small Projects](https://schroer.ca/2022/04/10/the-joy-of-small-projects/)
by Dominick Schroer

1. Pick the best project you can think of
2. Pick a very aggressive and limiting timeline
3. Reduce the project to its absolute minimum
4. Execute that plan to completion

&nbsp;

## [Do Things that Don't Scale](http://paulgraham.com/ds.html)
by [Paul Graham](https://twitter.com/paulg)

One of the most common types of advice we give at Y Combinator is to do things that don't scale.

&nbsp;

## [Shape Up: Stop Running in Circles and Ship Work that Matters](https://basecamp.com/shapeup)
by Basecamp

Shape Up is for product development teams who struggle to ship. If you’ve thought to yourself “Why can’t we ship like we used to?” or “I never have enough time to think about strategy,” then this book can help. You’ll learn language and techniques to define focused projects, address unknowns, and increase collaboration and engagement within your team.

&nbsp;

## [Overengineering can kill your product](https://www.mindtheproduct.com/overengineering-can-kill-your-product/)
by [Simón Muñoz](https://twitter.com/simonvlc)

The graveyard is filled with exquisitely designed startups and products to scale to millions of users who never got the slightest bit of traction. Don’t become one of them.

\includepdf[pages=1]{intermission.pdf}

# Stories

&nbsp;

## [Half a Billion in Bitcoin, Lost in the Dump](https://www.newyorker.com/magazine/2021/12/13/half-a-billion-in-bitcoin-lost-in-the-dump)
by [D.T. Max](https://twitter.com/dtmax)

For years, a Welshman who threw away the key to his cybercurrency stash has been fighting to excavate the local landfill.

&nbsp;

## [The Art of Code - Dylan Beattie](https://www.youtube.com/watch?v=6avJHaC3C2U)
by [Dylan Beattie](https://twitter.com/dylanbeattie)

But what about the code that only exists because somebody wanted to write it? Code created just to make people smile, laugh, maybe even dance? Maybe even code that does nothing at all, created just to see if it was possible?

&nbsp;

## [Inventing on Principle](https://www.youtube.com/watch?v=PUv66718DII)
by [Bret Victor](https://twitter.com/worrydream)

This is one of my favorite talks about programming. Bret Victor is a visionary and you should watch this talk.

&nbsp;

## [The Future of Programming](https://www.youtube.com/watch?v=8pTEmbeENF4)
by [Bret Victor](https://twitter.com/worrydream)

I would make you a disservice if I didn't link the other one by Bret. This talk is from 1973, or is it?

&nbsp;

## [A Brief Rant on the Future of Interaction Design](http://worrydream.com/ABriefRantOnTheFutureOfInteractionDesign/)
by [Bret Victor](https://twitter.com/worrydream)

With an entire body at your command, do you seriously think the Future Of Interaction should be a single finger?

&nbsp;

## [Edsger Dijkstra—The Man Who Carried Computer Science on His Shoulders](https://inference-review.com/article/the-man-who-carried-computer-science-on-his-shoulders)
by [Krzysztof R. Apt](https://homepages.cwi.nl/~apt/)

A pioneer whose work shaped his field like few others.

&nbsp;

## [The Cold War Bunker That Became Home to a Dark-Web Empire](https://www.newyorker.com/magazine/2020/08/03/the-cold-war-bunker-that-became-home-to-a-dark-web-empire)
by [Ed Caesar](https://twitter.com/edcaesar)

An eccentric Dutchman began living in a giant underground facility built by the German military—and ran a server farm beloved by cybercriminals.

&nbsp;

## [Growing a Language](https://www.youtube.com/watch?v=_ahvzDzKdB0)
by [Guy Steele](https://en.wikipedia.org/wiki/Guy_L._Steele_Jr.)

Guy Steele's keynote at the 1998 ACM OOPSLA conference on "Growing a Language" discusses the importance of and issues associated with designing a programming language that can be grown by its users.

&nbsp;

## [I programmed some creatures. They Evolved](https://www.youtube.com/watch?v=N3tRFayqVtk)
by [Dave Miller](http://www.millermattson.com/dave/)

This is a report of a software project that created the conditions for evolution in an attempt to learn something about how evolution works in nature. This is for the programmer looking for ideas for interdisciplinary programming projects, or for anyone interested in how evolution and natural selection work.

&nbsp;

## [To the Moon](https://www.youtube.com/watch?v=l3XwpSKqNZw)
by [Russ Olsen](https://twitter.com/russolsen)

We all have moments that change the way we think, the way that we look at the world, the things we want to do with our lives. On July 20, 1969 a whole generation of Americans had one of those transforming experiences: Two men landed on the Moon and nothing was ever the same again. 

&nbsp;

## [Vue.js: The Documentary](https://cult.honeypot.io/originals/vue-js-the-documentary)
by Honeypot

What began as a side project of a Google developer now shares the JS leaderboard with React and Angular. With the help of Sarah Drasner, Taylor Otwell, Thorsten Lünborg and many others from the Vue.js community, Evan You tells the story of how he fought against the odds to bring Vuejs to life.

&nbsp;

## [Laravel Origins: The Documentary](https://www.youtube.com/watch?v=127ng7botO4)
by OfferZen

Featuring Laravel creator Taylor Otwell and many others who’ve contributed to making Laravel the technology and community that it is today, Laravel Origins tells the story of why Laravel came to be, how it's grown over the last 10 years and what the future may hold for Taylor and the wider Laravel community.

&nbsp;

## [How Imaginary Numbers Were Invented](https://www.youtube.com/watch?v=cUzklzVXJwo)
by [Veritasium](https://twitter.com/veritasium)

A general solution to the cubic equation was long considered impossible, until we gave up the requirement that math reflect reality.

&nbsp;

## [Understanding the bin, sbin, usr/bin , usr/sbin split](http://lists.busybox.net/pipermail/busybox/2010-December/074114.html)
by [Rob Landley](https://twitter.com/landley)

When the operating system grew too big to fit on the first RK05 disk pack (their root filesystem) they let it leak into the second one, which is where all the user home directories lived (which is why the mount was called /usr).  They replicated all the OS directories under there (/bin, /sbin, /lib, /tmp...) and wrote files to those new directories because their original disk was out of space.

# Design

&nbsp;

## [Reviewing your design portfolios!](https://www.youtube.com/watch?v=2cn6wzH1xMo&list=PLrJQSKQvgHS5P_0m5DjXfDUMq2uW_3r_g)
by [Charli Marie](https://twitter.com/charliprangley)

I hope you enjoy getting a look at some awesome viewers work, and hearing the advice I give them! My hope with this series is that even if it's not your portfolio being reviewed you can get some useful insights that perhaps you could apply to your own design portfolio. I'm not critiquing the work in it, but rather the design and layout of your portfolio itself and looking critically at the type of work included to see if it aligns with your goals. I'd love to hear what you think of the series in the comments!

&nbsp;

## [UX request: Tell, don’t hide](https://jessitron.com/2020/11/30/ux-request-tell-dont-hide/)
by [Jessica Kerr](https://twitter.com/jessitron)

Y’know how sometimes a particular logged-in user isn’t authorized for some option, so you don’t show it to them?

Don’t do that to people. Tell me it’s there, tell me I can’t do it, tell me who can.

&nbsp;

## [Pride Backgrounds](https://www.dropbox.com/sh/m5kde7lj3ffqfi2/AAAtWTJ6g_rVCOQjGevl8CINa?dl=0)
by [Gosia Nowak](https://dribbble.com/designaur)

Happy Pride, folks!

&nbsp;

## [Christopher Alexander: A Primer](https://vimeo.com/491222729)
by [Ryan Singer](https://twitter.com/rjs)

Christopher Alexander’s work is hard to get into. He’s written over 15 books, and there isn't one that serves as a general intro or overview for the rest. In this livestream, I gave an informal introduction to what I think are the most important ideas in his body of work.

&nbsp;

## [How Designers Can Prevent User Errors](https://uxtools.co/blog/how-designers-can-prevent-user-errors)
by [Jordan Bowman](https://twitter.com/jrdnbwmn)

The term “user error” implies that it’s the user’s fault when they do something wrong.

But in the vast majority of cases, the fault actually rests with the designer for having created an interface that is confusing or makes it too easy for the user to make a mistake.

&nbsp;

## [7 Practical Tips for Cheating at Design](https://medium.com/refactoring-ui/7-practical-tips-for-cheating-at-design-40c736799886)
by [Adam Wathan](https://twitter.com/adamwathan) & [Steve Schoger](https://twitter.com/steveschoger)

It’s easy to throw your hands up and say, “I’ll never be able to make this look good, I’m not an artist!” but it turns out there are a ton of tricks you can use to level up your work that don’t require a background in graphic design.

&nbsp;

## [The Right Space Around Headings in Web Typography](https://www.youtube.com/watch?v=vBEIJa6KJ-Y)
by [Oliver Schöndorfer](https://twitter.com/glyphe)

We dive into the spacing around his headings, to structure his website better, talk about font sizes for headings, how to style captions, and I adore his marginalia and favicon.

# Css

&nbsp;

## [How to Find and Remove Dead CSS](https://blog.testdouble.com/talks/2021-06-03-how-to-find-and-remove-dead-css/)
by [Justin Searls](https://twitter.com/searls)

Do you have a pile of old CSS styles that you’re pretty sure are no longer referenced anywhere, but that you’re nevertheless afraid to delete because you have no way to be sure that no musty corners of your site somehow depend on them to render correctly?

&nbsp;

## [CSS Utility Classes and "Separation of Concerns"](https://adamwathan.me/css-utility-classes-and-separation-of-concerns/)
by [Adam Wathan](https://twitter.com/adamwathan)

Why “separation of concerns” is the wrong way to think about CSS and why presentational classes scale better than semantic classes.

&nbsp;

## [Flexbox Zombies](https://geddski.teachable.com/p/flexbox-zombies)
by [Dave Geddes](https://twitter.com/geddski)

Flexbox is incredibly powerful. But it's also crazy hard to learn well. So we all end up depending on a cheat sheet and some mad guessing in the dev tools.

This is an Educational Game. Each section unravels part of the plot, gives you expertise over a new flexbox concept, and presents zombie survival challenges that force you to solidify your new skills like your life depends on it.

&nbsp;

## [10 modern layouts in 1 line of CSS](https://www.youtube.com/watch?v=qm0IfG1GyZU)
by [Una Kravets](https://twitter.com/una)

In this dynamic talk, Una goes over the power of modern CSS layout techniques by highlighting a few key terms and how much detail can be described in a single line of code. Learn a few layout tricks you can implement in your codebase today, and be able to write entire swaths of layout with just a few lines of code.

&nbsp;

## [Tailwind CSS Tips, Tricks & Best Practices](https://www.youtube.com/watch?v=nqNIy8HkEQ8)
by [Sam Selikoff](https://twitter.com/samselikoff)

# Git

&nbsp;

## [Git's Best And Most Unknown Feature](https://www.youtube.com/watch?v=2uEqYw-N8uE)
by [ThePrimeagen](https://www.youtube.com/c/ThePrimeagen/about)

HOW HAVE I NOT HEARD OF GIT WORK TREES??? WHAT THE EFF.  They are so incredible.  You have to check them out!!! In this video I go over them briefly, assuming you are smart enough to understand them, and also show you my workflow with vim!  Its fantastic!

&nbsp;

## [13 Advanced (but useful) Git Techniques and Shortcuts](https://www.youtube.com/watch?v=ecK3EnyGD8o)
by Fireship

Productive programmers tend to be really good at Git. Take a look at 13 advanced git tips and tricks to supercharge your development workflow.

&nbsp;

## [Oh My Git!](https://ohmygit.org)
by [bleeptrack](https://twitter.com/bleeptrack) and [blinry](https://twitter.com/blinry)

An open source game about learning Git!

&nbsp;

## [Little Things I Like to Do with Git](https://csswizardry.com/2017/05/little-things-i-like-to-do-with-git/)
by [Harry Roberts](https://twitter.com/csswizardry)

I thought I would note down some useful little Git snippets that I use the most frequently.

&nbsp;

## [Strategies For Small, Focused Pull Requests](https://artsy.github.io/blog/2021/03/09/strategies-for-small-focused-pull-requests/)
by [Steve Hicks](https://twitter.com/pepopowitz)

A common suggestion for improving pull requests (PRs) is to "make your PR small and focused". I myself gave this suggestion in a recent article on this very blog about including context in PRs.

Like most internet advice, this can feel like the "draw the rest of the owl" meme. Even if we're in agreement that I should make a PR smaller...how do I do it? How do I avoid a big PR when there's a lot of cross-cutting changes to make? How do I create small, focused units of work when I'm building a large feature? How can I overcome my perfectionism and submit a PR that feels incomplete to me because the edges aren't all polished?

&nbsp;

## [Learn Git Branching](https://learngitbranching.js.org/)
by [Peter Cottle](https://twitter.com/petermcottle)

"Learn Git Branching" is the most visual and interactive way to learn Git on the web; you'll be challenged with exciting levels, given step-by-step demonstrations of powerful features, and maybe even have a bit of fun along the way.

# Tools

&nbsp;

## [An Introduction to JQ](https://earthly.dev/blog/jq-select/)
by [Adam Gordon Bell](https://twitter.com/adamgordonbell)

In this article, I’m going to go over the basics building blocks of jq in enough depth that you will be able to understand how jq works. Of course, you still might occasionally need to head to google to find a function name or check your syntax, but at least you’ll have a firm grounding in the basics.

&nbsp;

## [Developer Tools secrets that shouldn’t be secrets](https://christianheilmann.com/2021/11/01/developer-tools-secrets-that-shouldnt-be-secrets/)
by [Chris Heilmann](https://twitter.com/codepo8)

This is a talk that I’ve given at CityJS this September. I am a principal product manager for developer tools in Microsoft Edge and these are things I encountered during working on the tools, documenting them and going through user feedback.

&nbsp;

## [ShellCheck](https://www.shellcheck.net)
by [Vidar Holen](https://www.vidarholen.net)

Finds bugs in your shell scripts.

&nbsp;

## [Ray.so](https://ray.so)
by [Nichlas W. Andersen](https://twitter.com/nichlaswa)

Turn your code into beautiful images. Choose from a range of syntax colors, hide or show the background, and toggle between a dark and light window.

&nbsp;

## [Visualizing a codebase](https://next.github.com/projects/repo-visualization)
by [Amelia Wattenberger](https://twitter.com/Wattenberger)

How can we “fingerprint” a codebase to see its structure at a glance? Let’s explore ways to automatically visualize a GitHub repo, and how that could be useful.

&nbsp;

## [My workflow using Vim, 2021](https://www.youtube.com/watch?v=2WPC8rZQvQU)
by [ThePrimeagen](https://twitter.com/ThePrimeagen)

This is my workflow using vim. I wanted the kind of showcase how I jump around and how I think about things. this isn't meant to be a super technical video, more a video just describing how I approach using vim.

&nbsp;

## [Spying on your programs with strace](https://wizardzines.com/zines/strace/)
by [Julia Evans](https://twitter.com/b0rk)

strace is my favorite program. I think that it doesn’t get enough attention from programmers, so I wrote a zine about it to teach more people about how to use it.

&nbsp;

## [Understanding AWK](https://earthly.dev/blog/awk-examples/)
by [Adam Gordon Bell](https://twitter.com/adamgordonbell)

If you read through the article and maybe even try an example or two, you should have no problem writing Awk scripts by the end of it.

&nbsp;

## [cheat.sh](https://cheat.sh)
by [Igor Chubin](https://twitter.com/igor_chubin)

Unified access to the best community driven cheat sheets repositories of the world

&nbsp;

## [ShortcutFoo](https://www.shortcutfoo.com)

ShortcutFoo was created by programmers for programmers in an attempt to make learning your editor fun, easy, and effective. Akin to the days of first learning how to type on a keyboard, shortcutFoo aims to help programmers accomplish more in less time and with fewer keystrokes.

&nbsp;

## [Security Headers scanner](https://securityheaders.com)
by [Scott Helme](https://twitter.com/Scott_Helme)

There are services out there that will analyse the HTTP response headers of other sites but I also wanted to add a rating system to the results. The HTTP response headers that this site analyses provide huge levels of protection and it's important that sites deploy them. Hopefully, by providing an easy mechanism to assess them, and further information on how to deploy missing headers, we can drive up the usage of security based headers across the web.

&nbsp;

## [exa](https://the.exa.website)
by [Ben S](https://twitter.com/cairnrefinery)

A modern replacement for ls.

You list files hundreds of times a day. Why spend your time squinting at black and white text?

&nbsp;

## [Include diagrams in your Markdown files with Mermaid](https://github.blog/2022-02-14-include-diagrams-markdown-files-mermaid/)
by [Martin Woodward](https://twitter.com/martinwoodward) and [Adam Biagianti](http://availableforfriendship.com)

A picture tells a thousand words, but up until now the only way to include pictures and diagrams in your Markdown files on GitHub has been to embed an image. We added support for embedding SVGs recently, but sometimes you want to keep your diagrams up to date with your docs and create something as easily as doing ASCII art, but a lot prettier.

&nbsp;

## [Text Aesthetics: Command Line UI/UX](https://blog.mikecordell.com/2022/01/02/text-aesthetics-command-line-ui-ux/)
by [Michael Cordell](https://twitter.com/mike_cordell)

In this post I’ll show the many ways you can customize your command line. Through this customization, you can have a more pleasant and efficient experience while retaining the power and flexibility of text-only software. We will start with the terminal emulator, the main window to your CLI. We’ll move on to the text and colors that style the interface. We’ll close with using prompts and status lines to bring context to your text interface.

&nbsp;

## [Github Copilot MAKES A CLI GAME IN GOLANG FROM SCRATCH?!?!](https://www.youtube.com/watch?v=Xw_qbJp52cY)
by [ThePrimeagen](https://twitter.com/ThePrimeagen)

Yes the dang AI MAKES A GAME!!! I could not believe it and the ending just blew me away.

# Fun Stuff

&nbsp;

## [Proposal: Downward assignments](https://bugs.ruby-lang.org/issues/17768)
by [Yusuke Endoh](https://mametter.hatenablog.com)

Rightward assignments have been introduced since 3.0.
To be honest, I'm not a big fan of the syntax because it does not add a new dimension to Ruby.
Why don't we bring Ruby to the next dimension?


&nbsp;

## [Blob Opera](https://artsandculture.google.com/experiment/blob-opera/AAHWrq360NcGbw)
by [David Li](https://twitter.com/daviddotli)

Create your own opera inspired song with Blob Opera - no music skills required! A machine learning experiment by David Li in collaboration with Google Arts & Culture

&nbsp;

## [If PHP Were British](https://aloneonahill.com/blog/if-php-were-british/)
by [Dave Child](https://twitter.com/Dave_Child)

PHP developers in Britain have been grumpy about this ever since. What was he thinking? And more importantly, how do we undo this travesty? How do we developers ensure the traditions of the British Empire continue to be upheld, even in the digital age?

&nbsp;

## [Terms & Conditions Apply Game](https://termsandconditions.game/)
by [Wieden Kennedy](https://twitter.com/wklondon), [Jon Plackett](https://twitter.com/jonplackett), [Alex Bellos](https://twitter.com/AlexBellos), [Adam Hunt](https://twitter.com/iam_adam)

A mini-game about pop ups, and the deviousness of websites and apps
EVIL CORP wants your data. It will use every trick in the book (and a few more, just for fun).

&nbsp;

## [Sound of Colleagues](https://soundofcolleagues.com/)
by STHLM & Red Pipe Studios

We missed the sound of our colleagues so much that we created this website as a substitute.

&nbsp;

## [Wat](https://www.destroyallsoftware.com/talks/wat)
by [Gary Bernhardt](https://twitter.com/garybernhardt)

# Health

&nbsp;

## [Hands-Free Coding](https://joshwcomeau.com/accessibility/hands-free-coding/)
by [Joshua Comeau](https://twitter.com/joshwcomeau)

Earlier this year, I developed Cubital Tunnel Syndrome, a repetitive-strain injury, in both of my elbows. As a result, I pretty much can't use a mouse or keyboard; after a few minutes, I get a burning pain shooting down my arms. Even if I try to limit my computer usage to 60-second bursts, I wind up inadvertently making the situation worse.

&nbsp;

## [Reverse Engineering the source code of the BioNTech/Pfizer SARS-CoV-2 Vaccine](https://berthub.eu/articles/posts/reverse-engineering-source-code-of-the-biontech-pfizer-vaccine/)
by [Bert Hubert](https://twitter.com/PowerDNS_Bert)

Welcome! In this post, we’ll be taking a character-by-character look at the source code of the BioNTech/Pfizer SARS-CoV-2 mRNA vaccine.

&nbsp;

## [How Affordable Insulin Happened](https://www.juiceboxpodcast.com/episodes/jbp675)
by [Martin Van Trieste](https://twitter.com/MgVantrieste) on Juicebox Podcast

Martin Van Trieste is the President and Chief Executive Officer, Civica Rx. Civica is making affordable insulin.

&nbsp;

[The levels endorsement that can't be bought Betsy McLaughlin](https://mixtape.swyx.io/episodes/the-levels-endorsement-that-cant-be-bought-betsy-mclaughlin)
by [Swyx](https://twitter.com/swyx)

One CEO's struggle with her glucose levels despite trying everything.

# Career

&nbsp;

## [People don't work as much as you think](https://drmaciver.substack.com/p/people-dont-work-as-much-as-you-think)
by [David R. MacIver](https://twitter.com/DRMacIver)

If you do not realise this, and assume that everyone who says they are working eight hours per day actually is, you are probably going to wreck your mental health trying to keep up with them. Stop it at once.

&nbsp;

## [Write 5x more but write 5x less](https://critter.blog/2020/10/02/write-5x-more-but-write-5x-less/)
by [Mike Crittenden](https://twitter.com/mcrittenden)

&nbsp;

## [The biggest networking opportunity you're missing out on](https://www.youtube.com/watch?v=SJ3cXmRX7mM)
by [Mayuko](https://twitter.com/hellomayuko)

&nbsp;

## [Stop Looking For Mentors](https://staysaasy.com/career/2021/10/16/mentorship.html)
by [Stay SaaSy](https://twitter.com/staysaasy)

So stop worrying about mentors and mentorship and mentoring. Those words psyche people out. They set expectations that are counter to the normal time and mechanics it takes to build a relationship. 

&nbsp;

## [25 lessons from 25 years of coding](https://swizec.com/blog/25-lessons-from-25-years-of-coding/)
by [Swizec Teller](https://twitter.com/swizec)

Here's 25 lessons I've learned about code. In no particular order.

&nbsp;

## [Software development topics I've changed my mind on after 6 years in the industry](https://chriskiehl.com/article/thoughts-after-6-years)
by [Chris Kiehl](https://chriskiehl.com/about)

At some point, I realized I would've argued the exact opposite position on a lot of topics just a few years ago.

&nbsp;

## [Why Do We Work Too Much?](https://www.newyorker.com/culture/office-space/why-do-we-work-too-much)
by [Cal Newport](https://www.calnewport.com)

Our tendency to work twenty per cent too much is neither arbitrary nor sinister: it’s a side effect of the haphazard nature in which we allow our efforts to unfold. By thinking more intentionally about how work is identified, how it is prioritized, and how it is ultimately assigned, we can avoid some of the traps set by pure self-regulation.

&nbsp;

## [20 Things I’ve Learned in my 20 Years as a Software Engineer](https://www.simplethread.com/20-things-ive-learned-in-my-20-years-as-a-software-engineer/)
by [Justin Etheredge](https://twitter.com/JustinEtheredge)

My experiences over the last 20 years have shaped how I view software, and have led me to some beliefs which I’ve tried to whittle down to a manageable list that I hope you find valuable.

&nbsp;

## [Why Tacit Knowledge is More Important Than Deliberate Practice](https://commoncog.com/blog/tacit-knowledge-is-a-real-thing/)
by [Cedric Chin](https://twitter.com/ejames_c)

Once you understand that tacit knowledge exists, you will begin to see that big parts of any skill tree is tacit in nature, which means that you can go hunting for it, which in turn means you can start to ask the really useful question when it comes to expertise, which is: that person has it; that person is really good at it; how can I have it too?

&nbsp;

## [An Easier Method for Extracting Tacit Knowledge](https://commoncog.com/blog/an-easier-method-for-extracting-tacit-knowledge/)
by [Cedric Chin](https://twitter.com/ejames_c)

Let’s say that you’re a junior employee at a company, and you find a senior person with a set of skills that you’d like to learn. You ask them how they do it, and they give you an incomprehensible answer — something like “oh, I just know what to do”, or “oh, I just do what feels right”. How can you learn their skills?

&nbsp;

## [Expiring vs. Permanent Skills](https://www.collaborativefund.com/blog/expiring-vs-permanent-skills/)
by [Morgan Housel](https://twitter.com/morganhousel)

Robert Walter Weir was one of the most popular instructors at West Point in the mid-1800s. Which is odd at a military academy, because he taught painting and drawing.

&nbsp;

## [How You Know](http://www.paulgraham.com/know.html)
by [Paul Graham](https://twitter.com/paulg)

I've read Villehardouin's chronicle of the Fourth Crusade at least two times, maybe three. And yet if I had to write down everything I remember from it, I doubt it would amount to much more than a page. Multiply this times several hundred, and I get an uneasy feeling when I look at my bookshelves. What use is it to read all these books if I remember so little from them? (This made me feel better about myself.)

&nbsp;

## [On being wrong](https://www.ted.com/talks/kathryn_schulz_on_being_wrong)
by [Kathryn Schulz](https://twitter.com/kathrynschulz)

Most of us will do anything to avoid being wrong. But what if we're wrong about that? "Wrongologist" Kathryn Schulz makes a compelling case for not just admitting but embracing our fallibility.

&nbsp;

## [Life Audit](https://xsvengoechea.medium.com/how-and-why-to-do-a-life-audit-1d8bfbe1798)
by [Ximena Vengoechea](https://twitter.com/xsvengoechea)

Life audit (n.): An exercise in self-reflection that helps you clear the cobwebs of noisy, external goals and current distractions, and revisit or uncover the real themes & core values that drive & inspire you. Also known as: spring-cleaning for the soul.

&nbsp;

## [Personal values](https://www.julian.com/blog/life-planning)
by [Julian Shapiro](https://twitter.com/julian)

So I'm publicly sharing my framework to help others come to similar realizations: What should you really be working on?

&nbsp;

## [The paradox of choice](https://www.youtube.com/watch?v=VO6XEQIsCoM)
by [Barry Schwartz](https://twitter.com/BarrySch)

Psychologist Barry Schwartz takes aim at a central tenet of western societies: freedom of choice. In Schwartz's estimation, choice has made us not freer but more paralyzed, not happier but more dissatisfied.

&nbsp;

## [Deep Questions with Cal Newport Ep. 39: DAVID EPSTEIN on Skills, Practice, and the Subtle Art of Cultivating a Meaningful Career](https://www.calnewport.com/podcast/)
by [Cal Newport](https://www.calnewport.com)


&nbsp;

## [What designers NEED to know about IP, copyright & trademarks!](https://www.youtube.com/watch?v=UeeRCBf7mcY)
by [Charli Marie](https://twitter.com/charliprangley)

Intellectual property and your rights surrounding it can be a complex topic. In this video I'm breaking them down for you and giving you tips on things to watch out for. 

&nbsp;

## [Freelancing Full-Time](https://livedoinganything.com)
by Tommaso Manca

Making a living doing what you love is hard. On this website, you'll find resources to help you figure out how to make it happen in a way that suits you and actually makes the world better.

&nbsp;

## [Career checkup template](https://lethain.com/career-checkup/)
by [Will Larson](https://twitter.com/Lethain)

This is similar to a career narrative, but with more focus on self-diagnosis than something presentable to others.

&nbsp;

## [Rejecting Specialization](https://tomcritchlow.com/2022/06/16/rejecting-specialization/)
by [Tom Critchlow](https://twitter.com/tomcritchlow)

So in this post I’m going to reject the commonly accepted wisdom and look at why specializing is hard, why it fails and what an alternative path looks like. The answer lies in developing strong opinions and a distinctive vibe.

# Crypto

&nbsp;

## [NFTs Weren’t Supposed to End Like This](https://www.theatlantic.com/ideas/archive/2021/04/nfts-werent-supposed-end-like/618488/)
by [Anil Dash](https://twitter.com/anildash)

When we invented non-fungible tokens, we were trying to protect artists. But tech-world opportunism has struck again.

&nbsp;

## [Learn to Code Blockchain DApps By Building Simple Games](https://cryptozombies.io)
by CleverFlare

CryptoZombies is an interactive school that teaches you all things technical about blockchains. Learn to make smart contracts in Solidity or Libra by making your own crypto-collectibles game.

# Misc

&nbsp;

## [What Makes Quantum Computing So Hard to Explain?](https://www.quantamagazine.org/why-is-quantum-computing-so-hard-to-explain-20210608/)
by [Scott Aaronson](https://scottaaronson.com)

Quantum computers, you might have heard, are magical uber-machines that will soon cure cancer and global warming by trying all possible answers in different parallel universes. For 15 years, on my blog and elsewhere, I’ve railed against this cartoonish vision, trying to explain what I see as the subtler but ironically even more fascinating truth.

&nbsp;

## [Two Million Years in Two Hours: A Conversation with Yuval Noah Harari](https://www.humanetech.com/podcast/28-two-million-years-in-two-hours-a-conversation-with-yuval-noah-harari)
by [Center for Humane Technology](https://twitter.com/HumaneTech_)

Yuval Noah Harari is one of the rare historians who can give us a two-million-year perspective on today’s headlines. In this wide-ranging conversation, Yuval explains how technology and democracy have evolved together over the course of human history, from Paleolithic tribes to city states to kingdoms to nation states. 

So where do we go from here? “In almost all the conversations I have,” Yuval says, “we get stuck in dystopia and we never explore the no less problematic questions of what happens when we avoid dystopia.” We push beyond dystopia and consider the nearly unimaginable alternatives in this special episode of Your Undivided Attention.

&nbsp;

## [Patterns in confusing explanations](https://jvns.ca/blog/confusing-explanations/)
by [Julia Evans](https://twitter.com/b0rk)

So why do I find all these explanations so confusing? I decided to try and find out! I came up with a list of 13 patterns that make explanations hard for me to understand. For each pattern I’ll also explain what I like to do instead to avoid the issue.

&nbsp;

## [Understanding by Design](https://www.youtube.com/watch?v=4isSHf3SBuQ)
by [Grant Wiggins](https://twitter.com/grantwiggins)

Grant Wiggins introduces Understanding by Design (UbD), a framework for improving student achievement that helps teachers clarify learning goals, devise assessments that reveal student understanding, and craft effective learning activities.

&nbsp;

## [Emoji under the hood](https://tonsky.me/blog/emoji/)
by [Tonsky](https://twitter.com/nikitonsky)

For the past few weeks, I’ve been implementing emoji support for Skija. I thought it might be fun sharing a few nitty-gritty details of how this “biggest innovation in human communication since the invention of the letter A” works under the hood.

&nbsp;

## [Are We Really Engineers?](https://www.hillelwayne.com/post/crossover-project/are-we-really-engineers/)
by [Hillel Wayne](https://twitter.com/hillelogram)

Is software engineering “really” engineering? A lot of us call ourselves software engineers. Do we deserve that title? Are we mere pretenders to the idea of engineering?

&nbsp;

## [Why Electron apps are fine](https://nielsleenheer.com/articles/2021/why-electron-apps-are-fine/)
by [Niels Leenheer](https://twitter.com/html5test)

It is not difficult to find some incredibly shitty takes on Electron, and every time it boils down to: It’s slow. Downloads are huge, and it uses a lot of memory. Electron apps are just websites. Developers that are using Electron are taking the lazy or easy approach to cross-platform development. Native apps are just better in every single way. 

&nbsp;

## [The Stubborn Optimist’s Guide Revisited](https://www.humanetech.com/podcast/bonus-the-stubborn-optimists-guide-revisited)
by [Center For Humane Technology](https://twitter.com/HumaneTech_)

Internationally-recognized global leader on climate change Christiana Figueres argues that the battle against global threats like climate change begins in our own heads. She became the United Nations’ top climate official after she watched the 2009 Copenhagen climate summit collapse “in blood, in screams, in tears.” 

In the wake of that debacle, Christiana began performing an act of emotional Aikido on herself, her team, and eventually delegates from 196 nations. She called it “stubborn optimism.” It requires a clear and alluring vision of a future that can supplant the dystopian and discouraging vision of what will happen if the world fails to act. 

It was stubborn optimism, she says, that convinced those nations to sign the first global climate framework, the Paris Agreement. In this episode, we explore how a similar shift in Silicon Valley’s vision could lead three billion people to take action for the planet.

&nbsp;

## [Why Is Apple’s M1 Chip So Fast?](https://debugger.medium.com/why-is-apples-m1-chip-so-fast-3262b158cba2)
by [Erik Engheim](https://twitter.com/erikengheim/)

Real-world experience with the new M1 Macs has started ticking in. They are fast. Real fast. But why? What is the magic?

&nbsp;

## [The Beauty of Bézier Curves](https://www.youtube.com/watch?v=aVwxzDHniEw)
by [Freya Holmér](https://twitter.com/FreyaHolmer)

They're used for animation, text rendering, and all sorts of curved shapes! But how do they actually work? well, like, that's what the video is about, so, watch it to find out etc!!

&nbsp;

## [What the Version aka WTV?](https://gist.github.com/3v0k4/3625f3922e3035811e937155fd635e55)
by [yours truly](https://twitter.com/RiccardoOdone)

Version ranges in different languages: Ruby, JavaScript, Rust.

It's clear what >, >=, <, or <= mean. But what about those other cryptic symbols?

&nbsp;

## [Declarative Programming Streamers](https://declarative.tv)

We are a community of Functional and Declarative Programming streamers.

&nbsp;

## [Developer relations: (more than) the art of talking good](https://emilyfreeman.io/blog/developer-relations-more-than-the-art-of-talking-good)
by [Emily Freeman](https://twitter.com/editingemily)

In my opinion, someone in developer relations serves as an advocate for the tech community within their company. And we do a lot more than go to conferences.

&nbsp;

## [IT Burnout Index](https://burnoutindex.yerbo.co/survey)
by Yerbo

With the IT Burnout Index you can measure your own burnout risk levels at the present time, see the detailed result for each of the four burnout factors considered


\includepdf[pages=1]{intermission.pdf}
