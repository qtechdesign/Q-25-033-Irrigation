/** @type {import('next').NextConfig} */
const nextConfig = {
  output: 'export', // Static export for Cloudflare Pages
  images: {
    unoptimized: true,
  },
  trailingSlash: true,
}

module.exports = nextConfig

