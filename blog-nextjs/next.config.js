/** @type {import('next').NextConfig} */
const nextConfig = {
  images: {
    domains: ['cdn-images-1.medium.com'],
  },
  reactStrictMode: true,
  swcMinify: true,
  productionBrowserSourceMaps: true,
}

module.exports = nextConfig