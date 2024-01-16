/** @type {import('next').NextConfig} */
const nextConfig = {
  output: 'export',
  images: {
    domains: ['cdn-images-1.medium.com'],
  },
  reactStrictMode: true,
  swcMinify: true,
  productionBrowserSourceMaps: true,
  trailingSlash: true,
  transpilePackages: ['highlight.js'],
}

module.exports = nextConfig
