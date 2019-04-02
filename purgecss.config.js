class TailwindExtractor {
  static extract(content) {
    return content.match(/[A-Za-z0-9-_:\/]+/g) || [];
  }
}

module.exports = {
  content: [
    '_site/*.html',
    '_site/**/*.html'
  ],
  css: [
    process.env.TEMP_CSS_PATH
  ],
  extractors: [
    {
      extractor: TailwindExtractor,
      extensions: ['html', 'js', 'markdown', 'rst', 'hs']
    }
  ]
};
