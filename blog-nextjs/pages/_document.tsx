import { Html, Head, Main, NextScript } from 'next/document'

const Document = () => (
  <Html lang="en">
    <Head>
      <script async src="https://platform.twitter.com/widgets.js" charSet="utf-8"></script>
    </Head>
    <body>
      <Main />
      <NextScript />
    </body>
  </Html>
)

export default Document
