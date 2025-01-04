import Head from 'next/head'
import Link from 'next/link'
import Newsletter from '../components/newsletter'

const Tir = () => (
  <>
    <Head>
      <title>Debug your Time in Range - Riccardo Odone</title>
    </Head>

    <div className="py-20 mx-auto max-w-3xl px-4">
      <div className="text-center">
        <picture>
          <source type="image/webp" srcSet="/images/tir.webp" />
          <source type="image/jpeg" srcSet="/images/tir.png" />
          <img className="md:w-[50%] mx-auto" alt="Cover of a book: Debug your time in range - The 6-week course for diabetic developers to increase TIR by 10%" src="/images/tir.png" />
        </picture>
      </div>

      <h1 className="text-4xl font-semibold mb-4 leading-7 mt-32">
        Debug your time in range
        <br />
        <span className="text-xl text-gray-500">The 6-week course for diabetic developers to increase TIR by 10%</span>
      </h1>

      <p className="mt-10">
      This is also available as a <Link
        href="https://tir.odone.me"
        target="_blank"
        rel="noopener"
        className="underline text-[color:var(--blue)]">six-week email course</Link>.
      </p>

      <div className="mt-32 mb-5">
        <p className="mb-4 font-bold">
        How did you feel the last time you were out of range?
        </p>

        <p className="mb-4 font-bold">
        It sucked, huh?!
        </p>

        <p className="mb-4 mt-10">
        I’ve been a Type 1 for 25 years and I feel you: every injection, test, activity is yet another opportunity to screw it up.
        </p>

        <p className="mb-4">
        Remember the last time you rage bolussed a high and got low as a result? What about the time you ate the entire kitchen to correct a low and skyrocketed—and then rage bolussed?
        </p>

        <p className="mb-4">
        You tried so many times to reverse-engineer what the hell is going on. Even the exact same things that worked yesterday don’t work today.
        </p>

        <p className="mb-4">
        They told you that training is good for your diabetes, but forgot to say exercise makes it harder to control blood sugars.
        </p>

        <p className="mb-4">
        Let’s not mention weekends and days off are even worse than your week days.
        </p>

        <p className="mt-10 mb-4 font-bold">
        No shit, you are exhausted!
        </p>

        <hr className="mt-10" />

        <div className="mt-10 text-gray-500">
          <p className="mb-4">
          Ciao, this is Riccardo. I’ve been a type 1 for 25 years and a web developer for more than 10 years.
          </p>

          <p className="mb-4">
          I’m here to help you Increase your Time In Range by 10% in six weekly iterations.
          </p>

          <p className="mb-4">
          As a sofware developer, I like to keep it simple—boring if you will—and diabetes is no exception.
          </p>
        </div>
      </div>

      <h2 className="mt-32 mb-5 text-2xl uppercase text-center">
        Table of content
      </h2>

      <ul className="list-none p-0">
        <li>
          <a className="underline text-[color:var(--blue)]" href="#tip-0">
            The first step to 10% more Time In Range
          </a>
        </li>

        <li>
          <a className="underline text-[color:var(--blue)]" href="#tip-1">
            Tip #1: 100% basal insulin coverage
          </a>
        </li>

        <li>
          <a className="underline text-[color:var(--blue)]" href="#tip-2">
            Tip #2: TDD your blood sugars
          </a>
        </li>

        <li>
          <a className="underline text-[color:var(--blue)]" href="#tip-3">
            Tip #3: Remove <code>if</code>s from your diabetes management
          </a>
        </li>

        <li>
          <a className="underline text-[color:var(--blue)]" href="#tip-4">
            Tip #4: Bolusing is an impure function of timing and amount
          </a>
        </li>

        <li>
          <a className="underline text-[color:var(--blue)]" href="#tip-5">
            Tip #5: The KISS way to highs and lows
          </a>
        </li>

        <li>
          <a className="underline text-[color:var(--blue)]" href="#tip-6">
            Tip #6: This is how I do it
          </a>
        </li>
      </ul>

      <h2 id="tip-0" className="mt-32 mb-5 text-2xl uppercase text-center">
        The first step to 10% more Time In Range
      </h2>

      <p className="mb-4">
      As somebody who&apos;s been dealing with T1D for almost 25 years, I&apos;m inspired that you decided to take action. You rock!
      </p>

      <p className="mb-4">
      Please duplicate the <Link
        href="https://docs.google.com/spreadsheets/d/1agCSjgpFGCDfFSUt15aS5d9t3Voaqvf8qpk-Ri8m1AQ/edit?usp=sharing"
        className="underline text-[color:var(--blue)]"
        target="_blank"
        rel="noopener">spreadsheet</Link> you will use to track your 10% time in range increase in six weeks.
      </p>

      <p className="mb-4">
      Here&apos;s what you need to know about it:
      </p>

      <div className="text-center w-[100%] h-96 relative overflow-hidden">
        <iframe className="absolute top-0 left-0 w-[100%] h-[100%]" width="560" height="315" src="https://www.youtube.com/embed/heIyhPhwqFY" title="YouTube: The Spreadsheet" frameBorder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowFullScreen></iframe>
      </div>

      <p className="mt-10">
      I&apos;ll send you the first pro tip soon. In the meanwhile, please setup a reminder and track your blood sugars in the spreadsheet daily.
      </p>

      <h2 id="tip-1" className="mt-32 mb-5 text-2xl uppercase text-center">
        Tip #1: 100% basal insulin coverage
      </h2>

      <p className="mb-4">
      Have you ever worked in a codebase with a bad test coverage?
      </p>

      <p className="mb-4">
      You know, those projects with flaky tests failing randomly. Or a green suite that didn&apos;t catch a bug that later exploded on production. Or even complete lack of coverage in the area you are supposed to work on.
      </p>

      <p className="mb-4">
      Not only that gives you zero confidence, but it also destroys your motivation.
      </p>

      <p className="mt-10">
      Basal insulin is the same thing: with bad basal coverage you chase inexplicables lows and highs all day, with good basal coverage you have a stable flatline you can build upon.
      </p>

      <p className="mb-4">
      In other words, there&apos;s no way you can control your blood sugars if your basal insulin is not tuned in. Absolutely no way.
      </p>

      <p className="mt-10">
      <span className="italic">But how the hell do I fine tune my basal with all the variables that affect my blood sugars?</span> You test drive it—think unit testing where the unit under test is your basal insulin.
      </p>

      <p className="mb-4">
      Take an average day, be in a fasting state, drink only water, and measure your blood sugars every hour. You want your basal to keep your blood sugars in a flatline plus or minus a small variation (20-30 mg/dl). Here&apos;s a more in depth <Link
        href="https://integrateddiabetes.com/basal-testing/"
        className="underline text-[color:var(--blue)]"
        target="_blank"
        rel="noopener">explanation</Link>.
      </p>

      <p className="mt-10">
      Mark a day—or part of a day (e.g., night)—on the calandar and give basal testing a try. 
      </p>

      <h2 id="tip-2" className="mt-32 mb-5 text-2xl uppercase text-center">
        Tip #2: TDD your blood sugars
      </h2>

      <p className="mb-4">
      Would you rather fix a bug in a feature you deployed a few hours ago or a few days ago?
      </p>

      <p className="mb-4">
      You bet today&apos;s bug is the better pick! With the context fresh on your mind, you don&apos;t even need to look at the code to know what&apos;s wrong.
      </p>

      <p className="mt-10">
      With blood sugars, it&apos;s the same story: you want to debug them the same day and setup your tests for tomorrow—short feedback loops FTW!
      </p>

      <p className="mb-4">
      At the end of your day, jump into the <Link
        href="https://docs.google.com/spreadsheets/d/1agCSjgpFGCDfFSUt15aS5d9t3Voaqvf8qpk-Ri8m1AQ/edit?usp=sharing"
        className="underline text-[color:var(--blue)]"
        target="_blank"
        rel="noopener">Today sheet</Link>, fill in your readings, and focus on one or two surprising blood sugars: what can you do tomorrow to improve them? 
      </p>

      <p className="mb-4">
      In most cases, there are multiple explanations for a strange number. For example, waking up high could be a consequence of the dawn phenomenon, delayed dinner digestion, or not enough basal. Just pick the theory that sounds the most reasonable.
      </p>

      <p className="mt-10">
      Here&apos;s how I do it:
      </p>

      <div className="text-center w-[100%] h-96 relative overflow-hidden">
        <iframe className="absolute top-0 left-0 w-[100%] h-[100%]" width="560" height="315" src="https://www.youtube.com/embed/1o3rTq6tAQA" title="YouTube: Daily Review" frameBorder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowFullScreen></iframe>
      </div>

      <p className="mt-10">
      Remember, numbers are just numbers: they can be surprising but never good or bad. As with a broken CI or an issue on production, we fix it and try to do better next time. As difficult to believe as it sounds, we are not our numbers nor our bugs.
      </p>

      <h2 id="tip-3" className="mt-32 mb-5 text-2xl uppercase text-center">
        Tip #3: Remove <code>if</code>s from your diabetes management
      </h2>

      <p className="mb-4">
      Conditionals are the worst, aren&apos;t they?
      </p>

      <p className="mb-4">
      You add one innocent <code>if</code> and the code becomes twice as complex. Not to count, you have to double up the tests to cover all the paths.
      </p>

      <p className="mt-10">
      When calculating insulin, you run a ton of <code>if</code>s in your head. And I can guarantee one thing: you&apos;ll either make a mistake on the dose or on the timing—most likely, you&apos;ll be wrong on both. It&apos;s impossible to be as good as a pancreas, but here&apos;s the good news: you shouldn&apos;t even try.
      </p>

      <p className="mt-10">
      Don&apos;t strive for perfection: if you get to 85 mg/dl while shooting for 100 mg/dl you are golden. But how to reduce the outcome of the inevitable mistakes? You manage the variables.
      </p>

      <p className="mt-10">
      There are three types of variables that affect your blood sugars: the inevitables, the removables, and the ignorables.
      </p>

      <p className="mb-4">
      The ignorable variables are the ones you shouldn&apos;t even calculate. Fun fact, olives don&apos;t really raise my blood sugars, but YDMV (your diabetes may vary).
      </p>

      <p className="mb-4">
      The removable variables are the things you could avoid. For instance, I always had problems with training early in the morning. I prolly could have made it work, but working out later in the day fixed it.
      </p>

      <p className="mb-4">
      The inevitable variables are the ones that well.. you cannot avoid, but you can get to know them and limit their impact. For example, I tend to go low when I lie down during the day, so when I do that I eat a sugar cube.
      </p>

      <p className="mt-10">
      This week, take a look at your variables, analyze them, and develop a model on how to manage them. The best way to study a variable is the one you used for the basal test: remove all the others and focus on the one under test.
      </p>

      <p className="mt-10">
      PS: You won&apos;t like what comes next, but I&apos;d be dishonest if I didn&apos;t mention it: the biggest variable when it comes to diabetes is sugars and carbs. The less you consume, the easier is to manage your blood sugars.
      </p>

      <h2 id="tip-4" className="mt-32 mb-5 text-2xl uppercase text-center">
        Tip #4: Bolusing is an impure function of timing and amount
      </h2>

      <p className="mb-4">
      Wouldn&apos;t it be awesome if bolusing could be modeled with a pure function that takes a couple of arguments as an input and returns a unit amount as an output? You bet it would. 
      </p>

      <p className="mb-4">
      Unfortunately, bolusing comes with global state and side effects: hormones, physical activity, food volume, stressors, insulin site, digestion, and Lunar phases are just a few of the variables we deal with.
      </p>

      <p className="mt-10">
      On top of that, your diet and eating habits are unique to you: I cannot share tactics, but I can suggest a strategy.
      </p>

      <p className="mt-10">
      <span className="font-bold">
        1. Make sure your basal insulin is on spot
      </span>
      </p>

      <p className="mb-4">
      If you want to achieve good control, don&apos;t bother with fine tuning your bolus until you are stable in a fasting state. Basal testing will get you there!
      </p>

      <p className="mb-4">
      <span className="font-bold">
        2. Get the timing and amount of your bolus right
      </span>
      </p>

      <p className="mb-4">
      With the basal in check, you are halfway there—now you need to cover the short-term bumps. Imagine insulin as a curve that lowers your blood sugars, and food as a curve that raises them. You want those two curves to match as much as possible in amplitude and shape to nil each other.
      </p>

      <p className="mb-4">
      <span className="font-bold">
        2.A. Carbs are not all made equal
      </span>
      </p>

      <p className="mb-4">
      You could know the precise amount of carbs in a meal and your insulin to carb ratio, still it wouldn&apos;t be enough for optimal control—even if you magically removed all the other variables. Carbs are like temperature, you are interested in perceived degrees not absolute ones. And they don&apos;t impact all the same.
      </p>

      <p className="mb-4">
      In general, carbs act fast—some even faster. So you may want to pre-bolus and start eating as soon as insulin starts lowering your blood sugars to make the curves cancel out.
      </p>

      <p className="mb-4">
      <span className="font-bold">
        2.B. Proteins and fats count
      </span>
      </p>

      <p className="mb-4">
      Proteins and fats raise your blood sugars too. I&apos;m sure you witnessed it: you think you nailed the bolus but a second wave raises your blood sugars. Pizza anyone?
      </p>

      <p className="mb-4">
      Proteins and fats are converted to glucose but at a lower speed than carbs. Also, the less carbs you eat, the more you convert. Additionally, fats make you insulin resistant and slow down your digestion.
      </p>

      <p className="mb-4">
      You can use the <Link
        href="https://waltzingthedragon.ca/diabetes/nutrition-excercise/reduce-post-meal-spikes-caused-by-fat-and-protein/"
        className="underline text-[color:var(--blue)]"
        target="_blank"
        rel="noopener">Warsaw approach</Link> as a starting point. But, as a general rule, you cover proteins and fats with an extended bolus.
      </p>

      <p className="mb-4">
      <span className="font-bold">
        3. Be like water
      </span>
      </p>

      <p className="mb-4">
      Bruce Lee would have killed it as a diabetic:
      </p>

      <p className="pl-5 border-gray-400 text-gray-400" style={{ borderLeft: '4px solid' }}>
      Don’t get set into one form, adapt it and build your own, and let it grow, be like water. Empty your mind, be formless, shapeless — like water. Now you put water in a cup, it becomes the cup; You put water into a bottle it becomes the bottle; You put it in a teapot it becomes the teapot. Now water can flow or it can crash. Be water, my friend.
      </p>

      <p className="mb-4 mt-4">
      Global state is like hidden input to a function. If your basal insulin is fine tuned (tip #1), you create a good model to take care of your biggest variables (tip #3), and you develop the habit of daily retrospectives (tip #2), you expose global state and can manage it.
      </p>

      <h2 id="tip-5" className="mt-32 mb-5 text-2xl uppercase text-center">
        Tip #5: The KISS way to highs and lows
      </h2>

      <p className="mb-4">
      How does it feel to ship a bug to production? Bad, huh?!
      </p>

      <p className="mb-4">
      Well, actually it feels the same way as deploying correct code; it&apos;s only when you realize you shipped a bug that it stings.
      </p>

      <p className="mb-4">
      Now, let me ask you: how does it feel to inject the wrong dose of insulin?
      </p>

      <p className="mb-4">
      This is why we cannot focus only on the happy path: you and I know that some bugs will slip through together with some out-of-range blood sugars. Luckily, the goal is not perfection but a 10% time-in-range increase.
      </p>

      <p className="mb-4">
      And if you can manage 10, you can do even more.
      </p>

      <p className="mt-10">
      Before we get into highs and lows, there&apos;s one important thing: you need to be comfortable with your range. A number mid-range is as good as a number closer to the boundaries. Also, some fluctuations are expected—it&apos;s impossible to flatline.
      </p>

      <p className="mt-10">
      <span className="font-bold">The best way to handle a number out-of-range is not to get there in the first place</span>. And to do that you need one thing: flatten the curves.
      </p>

      <p className="mb-4">
      Imagine for a second trying to bolus a meal that raises you by 1000 mg/dl (56 mmol/l) almost instantly. You simply cannot do that while staying in range: insulin follows a parabolic curve of action, while the meal in case is a sudden spike. So you would need to choose between going (dangerously) low or going high. Also, making insulin work faster wouldn&apos;t help either, it would actually make things riskier.
      </p>

      <p className="mb-4">
      Now consider a meal that raises you by 250 mg/dl (14 mmol/l) over the course of four hours. Matching the raise with insulin is not easy (what is with diabetes?) but it&apos;s tractable. Most importantly, this situation is forgiving: even if the curves don&apos;t perfectly match—and they never will—you have all the time to correct and stay in range.
      </p>

      <p className="mb-4">
      I&apos;m not suggesting you to eat super–low-carb, or skip meaningful experiences that result in spikey curves—it&apos;s your choice. But some stuff will make your blood sugars more difficult to manage and you need to be mindful about it.
      </p>

      <p className="mb-4">
      Smaller input, smaller output. Some would say KISS.
      </p>

      <p className="mt-10">
      <span className="font-bold">The second best way to handle a number out-of-range is to bring it in as fast as possible</span>. If it&apos;s a low, you want to consume fast acting glucose that you can properly dose. If it&apos;s a high, you can make insulin work faster by injecting it intramuscularly, taking a warm bath, or going for a brisk walk.
      </p>

      <p className="mb-4">
      Do everything in your power to not over correct. It boils down to two choices: you can either stop the rollercoaster or perpetuate it. And we both know that a number out-of-range is frustrating, but a number-out-range after an over-correction feels like shit.
      </p>

      <p className="mb-4">
      Also, lows are not an excuse to eat something delicious. Focus first on regaining stability with a precise tool like sugar or dextrose before adding more curves. KISS, right?
      </p>

      <p className="mb-4">
      Finally, make sure you have glucagon and a trusted person within reach. Nobody likes to rollback a deploy, but you will be glad you prepared a plan b if the unexpected happens.
      </p>

      <h2 id="tip-6" className="mt-32 mb-5 text-2xl uppercase text-center">
        Tip #6: This is how I do it
      </h2>

      <p className="mb-4">
      This week, I turn the spotlight on myself and share with you how I do it—including controversial takes, strange tricks, and Italian hand gestures.
      </p>

      <p className="mb-4">
      I hope you can take some inspiration, but I encourage you to find your way. There are many people out there with better results than mine making different compromises and following a different strategy. With some hard work, I&apos;m sure you can tailor a lifestyle around your values, needs, and wishes.
      </p>

      <div className="text-center w-[100%] h-96 relative overflow-hidden">
        <iframe className="absolute top-0 left-0 w-[100%] h-[100%]" width="560" height="315" src="https://www.youtube.com/embed/OoR5lnyMyK4" title="YouTube: This is how I do it" frameBorder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowFullScreen></iframe>
      </div>

      <div className="mt-32 text-center">
        <picture>
          <source type="image/webp" srcSet="/images/tir-back.webp" />
          <source type="image/jpeg" srcSet="/images/tir-back.png" />
          <img className="md:w-[50%] mx-auto" alt="Backcover of the book: You are never alone inside a blue circle" src="/images/tir-back.png" />
        </picture>
      </div>

      <div className="mt-32 text-gray-500">
        <p className="mb-4">
        I (Riccardo Odone) am not a doctor. I share the perspective of somebody who has been dealing with type 1 diabetes 24/7 for 25 years. 
        Nothing that I say constitutes medical advice. Always consult with a real doctor before making any changes.
        </p>

        <p>
        Be a skeptic. I mean it.
        </p>
      </div>
    </div>

    <Newsletter />
  </>
)

export default Tir
