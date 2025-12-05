import Head from "next/head";
import Link from "next/link";
import Newsletter from "../components/newsletter";
import { InternalLink } from "../components/internal-link";
import { Tags, Props as TagsProps } from "../components/tags";
import { MicroTags, Props as MicroTagsProps } from "../components/micro-tags";
import { getCategories, getTags, getMicroCategories, getMicroTags, getAllPosts, getAllMicroPosts } from "./../lib/api";

type Posts = { latestPosts: { title: string, slug: string }[] };
type MicroPosts = { latestMicroPosts: { title: string, slug: string }[] };

const Home = ({ blog, microBlog }: { blog: TagsProps & Posts, microBlog: MicroTagsProps & MicroPosts }) => {
  return <>
    <Head>
      <title>Home - Riccardo Odone</title>
    </Head>

    <div className="hero relative bg-[position:70%_0%] bg-cover max-w-[1680px] mx-auto">
      <div className="px-[5%] text-white py-24 lg:bg-transparent lg:max-w-[50%] bg-black/[0.6]">
        <div className="slide-in">
          <h1 className="leading-tight uppercase text-5xl sm:text-6xl font-extralight">
            <p>Ciao, I&apos;m Riccardo!</p>
            <p className="mt-10">
              I craft web software and lead as a maverick.
            </p>
          </h1>
          <h2 className="text-4xl font-extralight mt-10">
            My favorite color is{" "}
            <span className="text-[#f76ca5] [text-shadow:1px_1px_1px_white]">
              pink
            </span>
            .
          </h2>
        </div>
      </div>
    </div>

    <div className="mt-20 mx-auto max-w-3xl px-4">
      <h2 className="text-3xl font-semibold mb-4">MicroBlogger</h2>
      <MicroTags categories={microBlog.categories} tags={microBlog.tags} />

      <h3 className="text-xl font-semibold uppercase mt-4">Latest</h3>
      <ul className="list-inside list-[circle]">
        { microBlog.latestMicroPosts.map(({ title, slug }) => (
          <li key={title}>
            <InternalLink href={`/micro-posts/${slug}`}>
              <a className="text-xl text-[color:var(--blue)] hover:text-[color:var(--pink)]">{title}</a>
            </InternalLink>
          </li>
        ))}
      </ul>
    </div>

    <div className="mt-20 mx-auto max-w-3xl px-4">
      <h2 className="text-3xl font-semibold mb-4">Blogger</h2>
      <Tags categories={blog.categories} tags={blog.tags} />

      <h3 className="text-xl font-semibold uppercase mt-4">Latest</h3>
      <ul className="list-inside list-[circle]">
        { blog.latestPosts.map(({ title, slug }) => (
          <li key={title}>
            <InternalLink href={`/posts/${slug}`}>
              <a className="text-xl text-[color:var(--blue)] hover:text-[color:var(--pink)]">{title}</a>
            </InternalLink>
          </li>
        ))}
      </ul>
    </div>

    <div className="mt-20 mx-auto max-w-3xl px-4">
      <h2 className="text-3xl font-semibold mb-4">Coder</h2>

      <div className="grid gap-20 grid-cols-[repeat(auto-fit,minmax(130px,1fr))]">
        <Link className="group" href="https://github.com/3v0k4/hotdocs" target="_blank" rel="noopener noreferrer">
          <img
            width="192"
            height="192"
            className="rounded-xl group-hover:scale-[103%]"
            alt="HotDocs icon: A humanized and happy hot dog."
            src="/images/hot_docs.svg"
          />
          <div className="text-sm mt-2 text-center">
            <p className="font-bold">HotDocs</p>
            <p>Write your docs with Ruby on Rails.</p>
          </div>
        </Link>

        <Link className="group" href="https://github.com/3v0k4/degem" target="_blank" rel="noopener noreferrer">
          <img
            width="192"
            height="192"
            className="rounded-xl group-hover:scale-[103%]"
            alt="Degem icon"
            src="/images/degem.svg"
          />
          <div className="text-sm mt-2 text-center">
            <p className="font-bold">Degem</p>
            <p>Find unused gems in a Ruby bundle.</p>
          </div>
        </Link>

        <Link className="group" href="https://github.com/3v0k4/exit.nvim" target="_blank" rel="noopener noreferrer">
          <img
            width="192"
            height="192"
            className="rounded-xl group-hover:scale-[103%]"
            alt="exit.nvim icon"
            src="/images/exit.svg"
          />
          <div className="text-sm mt-2 text-center">
            <p className="font-bold">exit.nvim</p>
            <p>I solved how to exit Vim. exit.nvim prompts LLMs to write neovim commands.</p>
          </div>
        </Link>

        <Link className="group" href="https://github.com/3v0k4/unpath" target="_blank" rel="noopener noreferrer">
          <img
            width="192"
            height="192"
            className="rounded-xl group-hover:scale-[103%]"
            alt="Unpath icon"
            src="/images/unpath.svg"
          />
          <div className="text-sm mt-2 text-center">
            <p className="font-bold">Unpath</p>
            <p>Runs a command with a modified PATH that does not contain the given command(s).</p>
          </div>
        </Link>

        <Link className="group" href="https://github.com/3v0k4/favicon_factory" target="_blank" rel="noopener noreferrer">
          <img
            width="192"
            height="192"
            className="rounded-xl group-hover:scale-[103%]"
            alt="Favicon Factory icon"
            src="/images/favicon_factory.svg"
          />
          <div className="text-sm mt-2 text-center">
            <p className="font-bold">Favicon Factory</p>
            <p>Generates from an SVG the minimal set of icons needed by modern browsers.</p>
          </div>
        </Link>

        <Link className="group" href="https://typescript.odone.me/" target="_blank" rel="noopener noreferrer">
          <img
            width="192"
            height="192"
            className="rounded-xl group-hover:scale-[103%]"
            alt="TypeScript.tips icon"
            src="/images/typescript-tips.svg"
          />
          <div className="text-sm mt-2 text-center">
            <p className="font-bold">TypeScript.tips</p>
            <p>Make bugs impossible. One TypeScript tip at a time.</p>
          </div>
        </Link>

        <Link className="group" href="https://rictionary.odone.me/" target="_blank" rel="noopener noreferrer">
          <img
            width="192"
            height="192"
            className="rounded-xl group-hover:scale-[103%]"
            alt="Rictionary icon"
            src="/images/rictionary.svg"
          />
          <div className="text-sm mt-2 text-center">
            <p className="font-bold">Rictionary</p>
            <p>My own personal Polish dictionary.</p>
          </div>
        </Link>

        <Link className="group" href="https://github.com/3v0k4" target="_blank" rel="noopener noreferrer">
          <img
            width="192"
            height="192"
            className="rounded-xl group-hover:scale-[103%] opacity-30"
            alt="GitHub icon"
            src="/images/logo-github.svg"
          />
          <div className="text-sm mt-2 text-center">
            <p className="font-bold">And more...</p>
            <p>Check out all my OSS on GitHub.</p>
          </div>
        </Link>
      </div>
    </div>

    <div className="mt-20 mx-auto max-w-3xl px-4">
      <h2 className="text-3xl font-semibold mb-4">Co-Organizer</h2>
      <ul className="list-disc list-inside">
        <li>
          <Link
            href="https://www.facebook.com/software.crafters.krakow"
            target="_blank"
            rel="noopener"
            className="underline text-[color:var(--blue)]"
          >
            Global Day of Coderetreat Kraków 2023/2022/2019
          </Link>
        </li>

        <li>
          <Link
            href="https://socrates-conf.de"
            target="_blank"
            rel="noopener"
            className="underline text-[color:var(--blue)]"
          >
            SoCraTes Unconference 2019/2018
          </Link>
        </li>
      </ul>
    </div>

    <div className="mt-20 mx-auto max-w-3xl px-4">
      <h2 className="text-3xl font-semibold mb-4">Speaker</h2>
      <ul className="list-disc list-inside">
        <li>
          <Link
            href="https://www.socrates-conference.de/"
            target="_blank"
            rel="noopener"
            className="underline text-[color:var(--blue)]"
          >
            SoCraTes
          </Link> — Just enough Python to beat Excel (<Link
            href="https://www.kaggle.com/code/riccardoodone/just-enough-python-to-beat-excel-socrates2024"
            target="_blank"
            rel="noopener"
            className="underline text-[color:var(--blue)]"
          >Jupyter Notebook</Link>)
        </li>

        <li>
          <Link
            href="https://www.futureconf.tech/"
            target="_blank"
            rel="noopener"
            className="underline text-[color:var(--blue)]"
          >
            Future Conf
          </Link> — TypeFirst TypeScript
        </li>

        <li>
          <Link
            href="https://www.meetup.com/tech-bytes-ocado-technology-krakow/events/293690403/"
            target="_blank"
            rel="noopener"
            className="underline text-[color:var(--blue)]"
          >
            Tech Bytes by Ocado
          </Link> — Bugs don’t exist (<Link
            href="https://www.youtube.com/watch?v=7o182OnK-LY"
            target="_blank"
            rel="noopener"
            className="underline text-[color:var(--blue)]"
          >video</Link>)
        </li>

        <li>
          <Link
            href="https://www.meetup.com/krakowjs/events/292600643/"
            target="_blank"
            rel="noopener"
            className="underline text-[color:var(--blue)]"
          >
            meet.js
          </Link> — TypeFirst TypeScript
        </li>

        <li>
          <Link
            href="https://www.youtube.com/@riccardoodone"
            target="_blank"
            rel="noopener"
            className="underline text-[color:var(--blue)]"
          >
            YouTube channel
          </Link>
        </li>
      </ul>
    </div>

    <div className="mt-20 mx-auto max-w-3xl px-4">
      <h2 className="text-3xl font-semibold mb-4">Diabetic</h2>

      <div className="flex flex-wrap gap-20 justify-between">
        <div className="basis-96 flex-grow">
          <InternalLink href="/tir">
            <a className="group">
              <picture>
                <source type="image/webp" srcSet="/images/tir.webp" />
                <source type="image/jpeg" srcSet="/images/tir.png" />
                <img
                  width="345"
                  height="533"
                  className="mx-auto rounded-xl group-hover:scale-[101%]"
                  alt="Cover of a book: Debug your time in range - The 6-week course for diabetic developers to increase TIR by 10%"
                  src="/images/tir.png"
                />
              </picture>
            </a>
          </InternalLink>
        </div>

        <div className="basis-40 flex-grow grid gap-20 grid-cols-[repeat(auto-fit,minmax(130px,1fr))] content-between">
          <div>
            <Link className="group" href="https://basal.odone.me" target="_blank" rel="noopener noreferrer">

              <picture>
                <source type="image/webp" srcSet="/images/basal.webp" />
                <source type="image/jpeg" srcSet="/images/basal.png" />
                <img
                  width="160"
                  height="160"
                  className="rounded-xl group-hover:scale-[102%]"
                  alt="Basal iPhone app icon"
                  src="/images/basal.png"
                />
              </picture>
              <div className="text-sm mt-2 text-center">
                <p className="font-bold">Basal iPhone App</p>
                <p>Plan, visualize, and perform basal insulin testing</p>
              </div>

            </Link>
          </div>

          <div>
            <Link className="group" href="https://dextop.odone.me" target="_blank" rel="noopener noreferrer">

              <picture>
                <source type="image/webp" srcSet="/images/dextop.webp" />
                <source type="image/jpeg" srcSet="/images/dextop.png" />
                <img
                  width="160"
                  height="160"
                  className="rounded-xl group-hover:scale-[102%]"
                  alt="DexTop app icon"
                  src="/images/dextop.png"
                />
              </picture>
              <div className="text-sm mt-2 text-center">
                <p className="font-bold">DexTop App</p>
                <p>Real-time Dexcom blood sugars on your desktop</p>
              </div>

            </Link>
          </div>
        </div>
      </div>
    </div>

    <div className="mt-20">
      <Newsletter />
    </div>
  </>;
};

export default Home;

export const getStaticProps = async () => {
  const latestPosts = getAllPosts(['title', 'slug']).slice(0, 3);
  const latestMicroPosts = getAllMicroPosts(['title', 'slug']).slice(0, 3);

  return {
    props: {
      blog: {
        categories: getCategories(),
        tags: getTags(),
        latestPosts,
      },
      microBlog: {
        categories: getMicroCategories(),
        tags: getMicroTags(),
        latestMicroPosts,
      }
    },
  };
};
