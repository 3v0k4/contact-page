import '../styles/base.css'
import '../styles/tailwind.css'
import type { AppProps } from 'next/app'
import Layout from '../components/layout'

const MyApp = ({ Component, pageProps }: AppProps) => (
  <Layout>
    <Component {...pageProps} />
  </Layout>
)

export default MyApp
