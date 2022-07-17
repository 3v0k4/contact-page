import Head from 'next/head'
import { InternalLink } from '../components/internal-link'
import Link from 'next/link'
import Newsletter from '../components/newsletter'
import { TagLink } from '../components/tag-link'
import { getCategories, getTags } from './../lib/api'

type Tag = {
  icon: string,
  tag: string,
  count: number,
}

type Category = Tag

type Props = {
  categories: Category[],
  tags: Tag[],
}

const Home = ({ categories, tags }: Props) => {
  return (
    <>
      <Head>
        <title>Riccardo Odone - Home</title>
      </Head>

      <div className="hero tw-relative tw-bg-[position:70%_0%] tw-bg-cover tw-bg-no-repeat">
        <div className="tw-px-[5%] tw-text-white tw-py-24 lg:tw-max-w-[50%] before:tw-top-0 before:tw-right-0 before:tw-left-0 before:tw-bottom-0 before:tw-bg-black/[0.2] before:tw-content-[''] before:tw-absolute md:before:tw-content-none">
          <h1 className="tw-leading-tight tw-uppercase tw-text-5xl sm:tw-text-6xl tw-font-extralight">
            <p>Ciao, I&apos;m Riccardo!</p>
            <p className="tw-mt-10">I craft web software and lead as a maverick.</p>
          </h1>
          <h2 className="tw-text-4xl tw-font-extralight tw-mt-10">
            My pronoun is he and I like <span className="tw-text-[#f76ca5] [text-shadow:1px_1px_1px_white]">pink</span>.
          </h2>
        </div>
      </div>

      <div className="col-sm-8 col-md-8 col-lg-6 mx-auto mt-5">
        <div className="tw-mt-5">
          <h2>PinkLetter</h2>
          <Link href="https://buttondown.email/riccardo.odone/archive"><a target="_blank" rel="noopener" className="tw-mt-4">Check the archives</a></Link>
        </div>

        <div className="tw-mt-5">
          <h2>Blog</h2>

          <div className="tw-mt-4">

            <div>
              { categories.map(({ icon, tag, count }) => (
                <TagLink key={tag} tag={tag}>
                  <span className="btn-tag-icon-lg">{icon} </span>{tag} <span className="badge badge-white">{count}</span>
                </TagLink>
              ))}
            </div>

            <div className="mt-3 d-none d-md-block">
              { tags.map(({ icon, tag, count }) => (
                <TagLink key={tag} tag={tag} klass="btn-sm">
                  <span className="btn-tag-icon-lg">{icon} </span>{tag} <span className="badge badge-white badge-sm">{count}</span>
                </TagLink>
              ))}
            </div>
          </div>
        </div>

        <div className="tw-mt-5">
          <h2>Courses</h2>

          <div className="tw-mt-4 tw-text-center">
            <InternalLink href={`/tir`}>
              <a>
                <picture>
                  <source type="image/webp" srcSet="/images/tir.webp" />
                  <source type="image/jpeg" srcSet="/images/tir.png" />
                  <img width="345" height="533" className="md:tw-w-[50%]" alt="Cover of a book: Debug your time in range - The 6-week course for diabetic developers to increase TIR by 10%" src="/images/tir.png" />
                </picture>
              </a>
            </InternalLink>
          </div>
        </div>

        <h2 className="mt-5">I&apos;m also on</h2>
        <ul className="no-list-style logos mt-4">
          <li>
            <Link href="https://www.youtube.com/channel/UCqoYTAX09Ico3T_NCRy-iSg"><a className="logo logo-youtube" target="_blank" rel="noopener">YouTube</a></Link>
          </li>

          <li>
            <Link href="https://twitter.com/riccardoodone"><a className="logo logo-twitter" target="_blank" rel="noopener">Twitter</a></Link>
          </li>

          <li>
            <Link href="https://dev.to/riccardoodone"><a className="logo logo-devto" target="_blank" rel="noopener">DevTO</a></Link>
          </li>

          <li>
            <Link href="https://medium.com/@riccardoodone"><a className="logo logo-medium" target="_blank" rel="noopener">Medium</a></Link>
          </li>

          <li>
            <Link href="https://www.facebook.com/riccardo.odone"><a className="logo logo-facebook" target="_blank" rel="noopener">Facebook</a></Link>
          </li>

          <li>
            <Link href="https://it.linkedin.com/in/riccardoodone"><a className="logo logo-linkedin" target="_blank" rel="noopener">Linkedin</a></Link>
          </li>

          <li>
            <Link href="https://github.com/3v0k4"><a className="logo logo-github" target="_blank" rel="noopener">Github</a></Link>
          </li>

          <li>
            <Link href="https://www.goodreads.com/review/list/75221217-riccardo?shelf=read&sort=rating"><a className="logo logo-goodreads" target="_blank" rel="noopener">GoodReads</a></Link>
          </li>
        </ul>
      </div>

      <Newsletter />
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
