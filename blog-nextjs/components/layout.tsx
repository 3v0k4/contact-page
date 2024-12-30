import Head from 'next/head'
import Link from 'next/link'
import React, { useRef, useLayoutEffect } from 'react'
import { InternalLink } from './internal-link'
import { useRouter } from 'next/router'
import goodreads from '../public/images/logo-goodreads.svg'
import facebook from '../public/images/logo-facebook.svg'
import github from '../public/images/logo-github.svg'
import linkedin from '../public/images/logo-linkedin.svg'
import youtube from '../public/images/logo-youtube.svg'
import twitter from '../public/images/logo-twitter.svg'
import devto from '../public/images/logo-devto.svg'
import medium from '../public/images/logo-medium.svg'
import Image from 'next/image';
import { TLD } from '../consts'

const Layout = ({ children }: React.PropsWithChildren<{}>) => {
  const router = useRouter()
  const canonical = router.asPath
  const footer = useRef<HTMLElement>(null)
  const selfie = useRef<HTMLAnchorElement>(null)
  useLayoutEffect(() => {
    if(!footer.current) return;
    if(!selfie.current) return;
    footer.current.scrollLeft = selfie.current.offsetLeft + // scroll to left edge of selfie
      selfie.current.offsetWidth / 2 - // scroll to center of selfie
      Math.max(document.documentElement.clientWidth, window.innerWidth || 0) / 2 // scroll back half the viewport
  })


  return <>
    <Head>
      <meta charSet="utf-8" />
      <meta httpEquiv="x-ua-compatible" content="ie=edge" />
      <meta name="viewport" content="width=device-width, initial-scale=1" />
      <link rel="apple-touch-icon" sizes="180x180" href="/images/apple-touch-icon.png" />
      <link rel="icon" type="image/png" sizes="32x32" href="/images/favicon-32x32.png" />
      <link rel="icon" type="image/png" sizes="16x16" href="/images/favicon-16x16.png" />
      <link rel="icon" type="image/png" sizes="192x192" href="/images/android-chrome-192x192.png" />
      <link rel="icon" type="image/png" sizes="512x512" href="/images/android-chrome-512x512.png" />
      <link rel="manifest" href="/site.webmanifest" />
      <meta name="twitter:site" content="@riccardoodone" />
      <meta name="twitter:creator" content="@riccardoodone" />
      <link rel="canonical" href={`https://${TLD}${canonical}`} />
      <meta property="og:url" content={`https://${TLD}${canonical}`} />
      <meta key="og:title" property="og:title" content="Riccardo Odone" />
      <meta key="og:description" property="og:description" content="Official website of Riccardo Odone. I craft web software and lead as a maverick. I teach and study timeless software skills on the blog." />
      <meta key="description" name="description" content="Official website of Riccardo Odone. I craft web software and lead as a maverick. I teach and study timeless software skills on the blog." />
      <meta key="og:type" property="og:type" content="website" />
      <meta key="og:image" property="og:image" content={`https://${TLD}/images/android-chrome-512x512.png`} />
      <meta key="twitter:card" name="twitter:card" content="summary" />
    </Head>

    <div className="flex flex-col">
      <div className="flex-grow-1 flex-shrink-0 basis-auto">
        <header>
          <div className="text-center">
            <InternalLink href="/">
              <a className="text-blue-diabetes hover:text-pinkk block mx-auto w-11 h-11 no-underline cursor-pointer">
                <span className="sr-only">Home</span>
                <svg fill="currentColor" viewBox="0 0 50 50" xmlns="http://www.w3.org/2000/svg">
                  <path fillRule="evenodd" clipRule="evenodd" d="M25 50C38.8071 50 50 38.8071 50 25C50 11.1929 38.8071 0 25 0C11.1929 0 0 11.1929 0 25C0 38.8071 11.1929 50 25 50ZM25 42.8571C34.8622 42.8571 42.8571 34.8622 42.8571 25C42.8571 15.1378 34.8622 7.14286 25 7.14286C15.1378 7.14286 7.14286 15.1378 7.14286 25C7.14286 34.8622 15.1378 42.8571 25 42.8571Z" />
                </svg>
              </a>
            </InternalLink>
          </div>
        </header>

        <main role="main">{children}</main>
      </div>

      <footer ref={footer} className="flex justify-between items-center p-4 mt-2 w-full max-w-3xl overflow-x-auto gap-6 md:gap-0 md:overflow-x-visible mx-auto">
        {[
          {
            href: "https://www.youtube.com/channel/UCqoYTAX09Ico3T_NCRy-iSg",
            src: youtube,
            label: "YouTube",
          },
          {
            href: "https://twitter.com/riccardoodone",
            src: twitter,
            label: "Twitter",
          },
          {
            href: "https://dev.to/riccardoodone",
            src: devto,
            label: "DevTO",
          },
          {
            href: "https://medium.com/@riccardoodone",
            src: medium,
            label: "Medium",
          },
        ].map(({ href, src, label }) => (
          <Link
            key={label}
            href={src}
            className="opacity-50 flex-shrink-0 flex justify-center items-center"
            target="_blank"
            rel="noopener">
            <Image className="h-14 w-14 hover:scale-110" width="56" height="56" src={src} alt={label} priority={true} />
          </Link>
        ))}

        <Link
          ref={selfie}
          href={'/'}
          className={`flex-shrink-0 block h-24 w-24 hover:scale-105`}>
          <picture>
            <source type="image/webp" srcSet="/images/selfie.webp" />
            <source type="image/png" srcSet="/images/selfie.png" />
            <img height="80" width="80" alt="Picture (headshot) of Riccardo Odone" className="rounded-full" src="/images/selfie.png" />
          </picture>
        </Link>

        {[
          {
            href: "https://www.facebook.com/riccardo.odone",
            src: facebook,
            label: "Facebook",
          },
          {
            href: "https://it.linkedin.com/in/riccardoodone",
            src: linkedin,
            label: "Linkedin",
          },
          {
            href: "https://github.com/3v0k4",
            src: github,
            label: "Github",
          },
          {
            href: "https://www.goodreads.com/review/list/75221217-riccardo?shelf=read&sort=rating",
            src: goodreads,
            label: "GoodReads",
          },
        ].map(({ href, src, label }) => (
          <Link
            key={label}
            href={src}
            className="opacity-50 flex-shrink-0 flex justify-center items-center"
            target="_blank"
            rel="noopener">
            <Image className="h-14 w-14 hover:scale-110" width="56" height="56" src={src} alt={label} priority={true} />
          </Link>
        ))}
      </footer>
    </div>
  </>;
}

export default Layout
