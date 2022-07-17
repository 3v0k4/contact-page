import Head from 'next/head'
import { InternalLink } from './internal-link'
import Link from 'next/link'
import React from 'react'
import hljs from 'highlight.js'
import { useRouter } from 'next/router'

const Layout = ({ children }: React.PropsWithChildren<{}>) => {
  React.useEffect(() => { hljs.highlightAll() })
  const router = useRouter()

  return (
  <>
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
      <link rel="canonical" href={`https://odone.io${router.asPath}`} />
      <meta property="og:url" content={`https://odone.io${router.asPath}`} />
      <meta property="og:title" content="Riccardo Odone" />
      <meta property="og:description" content="Official website of Riccardo Odone. I craft web software and lead as a maverick. I teach and study timeless software skills on the blog." />
      <meta name="description" content="Official website of Riccardo Odone. I craft web software and lead as a maverick. I teach and study timeless software skills on the blog." />
      <meta property="og:type" content="website" />
      <meta property="og:image" content="https://odone.io/images/android-chrome-512x512.png" />
      <meta name="twitter:card" content="summary" />
    </Head>

    <div className="tw-flex tw-flex-col">
      <div className="tw-flex-grow-1 tw-flex-shrink-0 tw-basis-auto">
        <header>
          <div className="tw-text-center">
            <InternalLink href="/"><a className="tw-block tw-mx-auto tw-w-12 tw-h-12 tw-no-underline tw-cursor-pointer tw-bg-[length:50px] tw-bg-[image:url('/images/android-chrome-512x512.png')]"><span className="tw-sr-only">Home</span></a></InternalLink>
          </div>
        </header>

        <main role="main">{children}</main>
      </div>

      <footer className="tw-text-center tw-py-4 tw-mt-2">
        <InternalLink href="/"><a><img height="80" width="80" alt="Picture (headshot) of Riccardo Odone" className="tw-w-20 tw-h-20 tw-rounded-full" src="/images/selfie.png" /></a></InternalLink>
        <h3 className="tw-mt-2">Gimme a shout on <Link href="https://twitter.com/riccardoodone"><a target="_blank" rel="noopener">Twitter</a></Link>!</h3>
      </footer>
    </div>
  </>
  )
}

export default Layout
