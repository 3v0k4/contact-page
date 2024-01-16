import Head from 'next/head'
import Link from 'next/link'
import React from 'react'
import hljs from 'highlight.js'
import { InternalLink } from './internal-link'
import { useRouter } from 'next/router'

const Layout = ({ children }: React.PropsWithChildren<{}>) => {
  React.useEffect(() => { hljs.highlightAll() })
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
            <InternalLink href="/"><a className="block mx-auto w-12 h-12 no-underline cursor-pointer bg-[length:50px] bg-[image:url('/images/android-chrome-512x512.png')]"><span className="sr-only">Home</span></a></InternalLink>
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
