import Head from 'next/head'
import Link from 'next/link'
import React from 'react'
import { InternalLink } from './internal-link'
import { useRouter } from 'next/router'

const Layout = ({ children }: React.PropsWithChildren<{}>) => {
  const router = useRouter()
  const canonical = router.asPath

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
      <link rel="canonical" href={`https://odone.io${canonical}`} />
      <meta property="og:url" content={`https://odone.io${canonical}`} />
      <meta property="og:title" content="Riccardo Odone" />
      <meta property="og:description" content="Official website of Riccardo Odone. I craft web software and lead as a maverick. I teach and study timeless software skills on the blog." />
      <meta name="description" content="Official website of Riccardo Odone. I craft web software and lead as a maverick. I teach and study timeless software skills on the blog." />
      <meta property="og:type" content="website" />
      <meta property="og:image" content="https://odone.io/images/android-chrome-512x512.png" />
      <meta name="twitter:card" content="summary" />
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

      <footer className="text-center py-4 mt-2">
        <InternalLink href="/">
          <a>
            <picture>
              <source type="image/webp" srcSet="/images/selfie.webp" />
              <source type="image/png" srcSet="/images/selfie.png" />
              <img height="80" width="80" alt="Picture (headshot) of Riccardo Odone" className="w-20 h-20 rounded-full mx-auto" src="/images/selfie.png" />
            </picture>
          </a>
        </InternalLink>
        <h3 className="mt-2 font-bold">Gimme a shout on <Link
          href="https://twitter.com/riccardoodone"
          target="_blank"
          rel="noopener"
          className="underline">Twitter</Link>!</h3>
      </footer>
    </div>
  </>;
}

export default Layout
