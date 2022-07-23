import Head from 'next/head'
import Link from 'next/link'
import Newsletter from '../components/newsletter'
import { InternalLink } from '../components/internal-link'
import { Tags, Props as TagsProps } from '../components/tags'
import { getCategories, getTags } from './../lib/api'

const Home = ({ categories, tags }: TagsProps) => {
  return (
    <>
      <Head>
        <title>Riccardo Odone - Home</title>
      </Head>

      <div className="hero relative bg-[position:70%_0%] bg-cover bg-no-repeat">
        <div className="px-[5%] text-white py-24 lg:bg-transparent lg:max-w-[50%] bg-black/[0.6]">
          <div className="slide-in">
            <h1 className="leading-tight uppercase text-5xl sm:text-6xl font-extralight">
              <p>Ciao, I&apos;m Riccardo!</p>
              <p className="mt-10">I craft web software and lead as a maverick.</p>
            </h1>
            <h2 className="text-4xl font-extralight mt-10">
              My pronoun is he and I like <span className="text-[#f76ca5] [text-shadow:1px_1px_1px_white]">pink</span>.
            </h2>
          </div>
        </div>
      </div>

      <div className="mt-20 mx-auto max-w-3xl px-4">
        <div>
          <h2 className="text-4xl font-semibold mb-4">PinkLetter</h2>
          <Link href="https://buttondown.email/riccardo.odone/archive">
            <a target="_blank" rel="noopener" className="underline text-[color:var(--blue)]">Check the archives</a>
          </Link>
        </div>

        <div className="mt-20">
          <h2 className="text-4xl font-semibold mb-4">Blog</h2>

          <Tags categories={categories} tags={tags} />
        </div>

        <div className="mt-20">
          <h2 className="text-4xl font-semibold mb-4">Courses</h2>

          <InternalLink href={`/tir`}>
            <a>
              <picture>
                <source type="image/webp" srcSet="/images/tir.webp" />
                <source type="image/jpeg" srcSet="/images/tir.png" />
                <img width="345" height="533" className="md:w-[50%] mx-auto" alt="Cover of a book: Debug your time in range - The 6-week course for diabetic developers to increase TIR by 10%" src="/images/tir.png" />
              </picture>
            </a>
          </InternalLink>
        </div>

        <div className="mt-20">
          <h2 className="text-4xl font-semibold mb-4">I&apos;m also on</h2>

          <ul className="list-none flex flex-wrap gap-8">
            {
              [
                { href: "https://www.youtube.com/channel/UCqoYTAX09Ico3T_NCRy-iSg", klass: "bg-[url(/images/logo-youtube.svg)]", label: "YouTube" },
                { href: "https://twitter.com/riccardoodone", klass: "bg-[url(/images/logo-twitter.svg)]", label: "Twitter" },
                { href: "https://dev.to/riccardoodone", klass: "bg-[url(/images/logo-devto.svg)]", label: "DevTO" },
                { href: "https://medium.com/@riccardoodone", klass: "bg-[url(/images/logo-medium.svg)]", label: "Medium" },
                { href: "https://www.facebook.com/riccardo.odone", klass: "bg-[url(/images/logo-facebook.svg)]", label: "Facebook" },
                { href: "https://it.linkedin.com/in/riccardoodone", klass: "bg-[url(/images/logo-linkedin.svg)]", label: "Linkedin" },
                { href: "https://github.com/3v0k4", klass: "bg-[url(/images/logo-github.svg)]", label: "Github" },
                { href: "https://www.goodreads.com/review/list/75221217-riccardo?shelf=read&sort=rating", klass: "bg-[url(/images/logo-goodreads.svg)]", label: "GoodReads" },
              ].map(({ href, klass, label }) => (
                <li key={href}>
                  <Link href={href}><a className={`block indent-[-9999px] h-14 w-14 bg-contain bg-no-repeat bg-center ${klass}`} target="_blank" rel="noopener">{label}</a></Link>
                </li>
            ))}
          </ul>
        </div>
      </div>

      <div className="mt-20">
        <Newsletter />
      </div>
    </>
  )
}

export default Home

export const getStaticProps = async () => {
  const categories = getCategories()
  const tags = getTags()

  return {
    props: { categories, tags },
  }
}
