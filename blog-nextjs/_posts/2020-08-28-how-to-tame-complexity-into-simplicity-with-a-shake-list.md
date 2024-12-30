---
title: How to Tame Complexity Into Simplicity with a Shake List
description: The trick is in minimizing the feedback loop. Ideally, you want to be told at each turn if you are wrong, not drive for days just to realize you were heading the opposite direction.
published: true
author: Riccardo
cover_image: /images/leaves.jpg
tags:
  - Essential Skills
---

*Nobody wanted to work on that ticket. It was an epic, an epic pain in the ass. But there's nothing more motivating than an impossible challenge, or at least that's what I thought.*

That's how I got to work on the most difficult feature of my career. The gap between the complexity of the task and personal skills was the biggest I have ever dealt with. Today, after a couple of years, I decided to **formalize the framework I used to tame the problem complexity into a simple solution**.

But watch out, self-confidence can bring you so far. Luckily, I had enough experience to deal with the task. But one thing I learned was to understand my limits, be ready to let go, and ask for help. Regardless of how good the framework is, it cannot make up for the lack of knowledge.

However, even when you possess the skills, you still need **structure to keep complexity under control**. Not having a process will result in a barely functioning spaghetti mess at best, or in total waste at worst. Moreover, devising a simple solution to a complex problem requires time. There are no magic bullets. But you can accomplish more, better, and faster by mustering some discipline.

**The secret is in minimizing the feedback loop**. Ideally, you want to be told at each turn if you are wrong, not drive for days just to realize you were heading the opposite direction. You need to catch mistakes soon to **avoid rework**. You want the tight feedback loop of TDD even before you put your fingers on a keyboard. And, as we will see, you should not touch a computer until late in the process.

## The Problem

At first, I could not understand why people deemed the task complex. It was a search page with some twenty filters on the left, a dropdown to apply sorting to the results, four different visualizations of data, a button to download matches in different formats, and an option to save the search with all the selected criteria.

Sure, it was a lot of stuff. However, splitting it up into smaller chunks would have solved the problem. But then it struck me: everything was interacting with everything else on the page. **It wasn't trivial to decompose** because foundations were shared. And, given the size of the dataset, performance needed to be taken into account from the ground up.

When I started clicking on the wireframes, things got more complicated. I opened some filters and found out they contained checkboxes. Each one displayed in parentheses the "associated results", in other words, how many results would be shown if that option got selected. A checkbox that would contribute to zero matches needed to be disabled.

Checking one option in one filter did not change its associated results but it did for the ones in other filters. That made me realize that each filter's criteria worked in an AND relationship and between filters it worked with an OR.

Several badges showed how many criteria were selected both for a single filter and globally. A "clear all" button allowed users to reset all the filters. Also, at the top of the screen, there was a summary of all the checks in the shape of tags that could be unselected with a click.

But it's when I opened the most sophisticated filter that I started sweating. Each checkbox contained nested checkboxes. By clicking on a small arrow, the user could drill down to the children. That is why there were two labels in each checkbox, the additional one represented the nesting depth. All levels above the current were represented as links to allow the user to navigate back. Checking one node selected itself and the successors (not only its direct children). Checking some but not all the checkboxes on a level made the parent checkbox "partially checked". Also, at the top of the filter, a text field allowed searching for checkboxes regardless of their nesting level.

This was what I discovered on the designs, then I started asking questions. In fact, on the wireframes things made sense, but, when I started simulating scenarios, edge cases popped up. At first, I was querying the client on the daily standups. Then we started having separate calls because I was monopolizing all the time. Here's one example of such questions:

> What if the user selects checkbox-1 in filter-A that has more than 0 associated results, then they select checkbox-2 in filter-B that makes the selected results of checkbox-1 go down to 0 since the latter is now disabled, how would the user uncheck it?

A few days in the making, somebody suggested that every few months the database got updated. Since the checkboxes were generated dynamically out of the dataset, it meant some would be added, removed, or updated. That had repercussions all over the board.

Trust me, I could go on for hours.

## The Shake List

That level of complexity required multiple iterations. In other words, I needed to **validate every candidate solution against all the features and constraints to avoid the risk of building the wrong stuff**. Therefore, after spending days staring at wireframes, talking to the client, and simulating scenarios, I condensed all into a Shake List.

Here's how it looked like:

1. Nested checkboxes
2. Deeply nested checkboxes
3. Data manipulation performance
4. Full checks
5. Save search
6. Partial checks
7. ...

The shake list marked the end of the problem exploration. Thus, I cracked my knuckles getting ready to type. Unfortunately, **you can code a solution not a problem** and right then I just understood the latter. So I grabbed the marker and walked back to the whiteboard.

I decided to tackle the most complex filter first. Since it combined all the features of the other ones, solving it meant solving all of them. Plus, I would have addressed the riskiest thing first which is always a good idea.

But the shake list was long. Too long to devise one solution to rule all the items. Therefore, I decided to gather candidate solutions that would each solve an individual problem. Also, I knew from Design Thinking that **creative work and refining work don't play along**. So, as a first step, I decided to consider all ideas brilliant and defer the critique.

Here's what I came up with:

1. Nested checkboxes -> Array of arrays OR matrix OR tree
2. *Deeply* nested checkboxes -> Eager OR lazy loading nodes
3. Data manipulation performance -> Same representation for data manipulation and user interface OR two different ones
4. Full checks -> Boolean flag in each node
5. Save search -> Extract all full checks and save them OR keep selected ids separately from the data structure representing the checkboxes so it's readily available to be saved
6. Partial checks -> Additional boolean flag OR use an enum with three values
7. ...

The time had come to put down the designer's hat and pick up the engineer's one. **I shook the candidate solutions against the shake list**. First of all, I discarded all the invalid solutions (e.g. didn't solve the problem, inferior to an alternative). Then, I marked candidate solutions that either solved more than one problem or were complementary to each other.

I kept shaking the tree glad of all the leaves falling down. They were the **silly ideas I saved myself from**. Also, I noted why any item got discarded for documentation's sake. Among other things, that proved to be useful when my brain wanted some solutions to work and kept surfacing them. Luckily, I could refer to my notes and avoid falling prey to the [Einstellung effect](/posts/2020-06-26-why-good-solutions-block-better-ones/).

Here's the list of candidate solutions after shaking:

1. Nested checkboxes -> Tree. Our checkboxes are clearly trees of nodes. Computer science developed a ton of algorithms and literature when it comes to this data structure. I'm going to get a ton of stuff for free by relying on it.
2. Deeply nested checkboxes -> Eager OR lazy loading nodes. Difficult to say giving the size of the dataset. On one hand, eager loading would make things easier but it would be creating more pressure on memory. Need to investigate more.
3. Data manipulation performance -> Two different representations. Separating the data representation from the UI representation means being able to optimize both at the small cost of adding a transformation step in between.
4. Full checks -> Boolean flag in each node.
5. Save search -> Keep an array of selected ids separated from the tree of checkboxes. They have different lifecycles and by keeping the two separated we honor it. This enables some cool tricks given the consolidating step mentioned in 3. For example, un-selecting a checkbox means removing one id from the array, clearing all selections means emptying the array.
6. Partial checks -> Enum with three values. This makes impossible states impossible. With two boolean flags, I would have four combinations when actually only three are legit: "checked", "partially checked", "unchecked".
7. ...

Despite all of my efforts, I still had some alternatives I couldn't trim down on the whiteboard. That's where I moved to the next low **fidelity tool that proved to be good enough for validation**. I relied on sketches to discuss a bunch of solutions with the client. Sometimes I stepped into clickable wireframes with the help of a designer. Some other times it was HTML and JavaScript. When it came to performance, I decided to implement a prototype in the language and environment of the project.

Back then, the shake list was endless and took me several rounds. But I managed to get to a solid solution without producing waste or spending time on hopeless ideas. Also, notice no language, framework or even environment have been mentioned so far. **Raising the analysis from specifics to computer science** not only removes noise, but also lifts the discussion to a more universal idiom and enables talking to more experts, regardless of their background, and access to more literature.

Great, it was finally time to code! Actually, not yet. Lo-fi prototyping was great at catching silly mistakes early but could not cover all the ground. Thus, I gave myself permission to code, but only for prototyping's sake. By not rushing I uncovered additional obstacles that tackled late would have been problematic. For example, I was planning to save searches by keeping all the selected ids in the URL but it turned out the character limit didn't allow it.

I explored the problem until I grokked it, I came up with validated and performant solutions, and I kept options open to survive the client's changes of mind. Right then, I laid my fingers on a keyboard to translate my solution into code with the confidence I was **building the right thing right**.

## Outro

Nowadays, I would have deferred coding more. You know what I would have done instead? You bet it is more whiteboarding. Candidate solutions were still too abstract. Today I would have [Decomposed the Feature Into a Pipeline](/posts/2020-06-29-decomposing-features-into-pipelines/) and made sure to get [Code Quality for Free](/posts/2020-08-20-code-quality-is-free-if-you-do-it-right/).

Besides consuming more markers, I would also overcommunicate more throughout the process. Unfortunately, it's **not common to see a developer spending time on the whiteboard**. Thus, it's better to raise the objection and explain what's going on. I highlight the complexity of the problem by making the shake list visible. I employ the process to propose shortcuts or better alternatives. I advertise the many silly mistakes I avoided. I show by example that my work is faster and better by the nature of my process.
