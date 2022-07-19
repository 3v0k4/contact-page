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

      <div className="hero tw-relative tw-bg-[position:70%_0%] tw-bg-cover tw-bg-no-repeat">
        <div className="tw-px-[5%] tw-text-white tw-py-24 lg:tw-bg-transparent lg:tw-max-w-[50%] tw-bg-black/[0.6]">
          <h1 className="tw-leading-tight tw-uppercase tw-text-5xl sm:tw-text-6xl tw-font-extralight">
            <p>Ciao, I&apos;m Riccardo!</p>
            <p className="tw-mt-10">I craft web software and lead as a maverick.</p>
          </h1>
          <h2 className="tw-text-4xl tw-font-extralight tw-mt-10">
            My pronoun is he and I like <span className="tw-text-[#f76ca5] [text-shadow:1px_1px_1px_white]">pink</span>.
          </h2>
        </div>
      </div>

      <div className="tw-mt-20 tw-mx-auto tw-max-w-3xl tw-px-4">
        <div>
          <h2 className="tw-text-4xl tw-font-semibold tw-mb-4">PinkLetter</h2>
          <Link href="https://buttondown.email/riccardo.odone/archive">
            <a target="_blank" rel="noopener" className="tw-underline tw-text-[color:var(--blue)]">Check the archives</a>
          </Link>
        </div>

        <div className="tw-mt-20">
          <h2 className="tw-text-4xl tw-font-semibold tw-mb-4">Blog</h2>

          <Tags categories={categories} tags={tags} />
        </div>

        <div className="tw-mt-20">
          <h2 className="tw-text-4xl tw-font-semibold tw-mb-4">Courses</h2>

          <InternalLink href={`/tir`}>
            <a>
              <picture>
                <source type="image/webp" srcSet="/images/tir.webp" />
                <source type="image/jpeg" srcSet="/images/tir.png" />
                <img width="345" height="533" className="md:tw-w-[50%] tw-mx-auto" alt="Cover of a book: Debug your time in range - The 6-week course for diabetic developers to increase TIR by 10%" src="/images/tir.png" />
              </picture>
            </a>
          </InternalLink>
        </div>

        <div className="tw-mt-20">
          <h2 className="tw-text-4xl tw-font-semibold tw-mb-4">I&apos;m also on</h2>

          <ul className="tw-list-none tw-flex tw-flex-wrap tw-gap-8">
            {
              [
                { href: "https://www.youtube.com/channel/UCqoYTAX09Ico3T_NCRy-iSg", klass: "tw-bg-[url(/images/logo-youtube.svg)]", label: "YouTube" },
                { href: "https://twitter.com/riccardoodone", klass: "tw-bg-[url(/images/logo-twitter.svg)]", label: "Twitter" },
                { href: "https://dev.to/riccardoodone", klass: "tw-bg-[url(/images/logo-devto.svg)]", label: "DevTO" },
                { href: "https://medium.com/@riccardoodone", klass: "tw-bg-[url(/images/logo-medium.svg)]", label: "Medium" },
                { href: "https://www.facebook.com/riccardo.odone", klass: "tw-bg-[url(/images/logo-facebook.svg)]", label: "Facebook" },
                { href: "https://it.linkedin.com/in/riccardoodone", klass: "tw-bg-[url(/images/logo-linkedin.svg)]", label: "Linkedin" },
                { href: "https://github.com/3v0k4", klass: "tw-bg-[url(/images/logo-github.svg)]", label: "Github" },
                { href: "https://www.goodreads.com/review/list/75221217-riccardo?shelf=read&sort=rating", klass: "tw-bg-[url(/images/logo-goodreads.svg)]", label: "GoodReads" },
              ].map(({ href, klass, label }) => (
                <li key={href}>
                  <Link href={href}><a className={`tw-block tw-indent-[-9999px] tw-h-14 tw-w-14 tw-bg-contain tw-bg-no-repeat tw-bg-center ${klass}`} target="_blank" rel="noopener">{label}</a></Link>
                </li>
            ))}
          </ul>
        </div>
      </div>

      <div className="tw-mt-20">
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
