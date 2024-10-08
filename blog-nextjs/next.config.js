/** @type {import('next').NextConfig} */
const nextConfig = {
  output: 'export',
  images: {
    domains: ['cdn-images-1.medium.com'],
    unoptimized: true,
  },
  reactStrictMode: true,
  swcMinify: true,
  productionBrowserSourceMaps: true,
  trailingSlash: true,
}

module.exports = nextConfig
