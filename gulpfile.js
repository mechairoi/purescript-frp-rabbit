'use strict';

var gulp        = require('gulp')
  , run         = require('gulp-run')
  , purescript  = require('gulp-purescript')
  , source      = require('vinyl-source-stream')
  , browserify  = require('browserify')
  , path        = require("path")
  ;

var paths = {
  src: 'src/**/*.purs',
  doc: 'MODULE.md',
  docSrc: ['src/**/*.purs', '!src/**/Internal/**/*.purs' ],
  bowerSrc: 'bower_components/purescript-*/src/**/*.purs',
  dest: 'output',
  example: 'examples/*.purs',
  test: 'test/**/*.purs'
};

gulp.task('make', function() {
  return gulp.src([paths.src].concat(paths.bowerSrc))
    .pipe(purescript.pscMake({}))
    .pipe(gulp.dest(paths.dest));
});

gulp.task('test-make', function() {
  return gulp.src([paths.src, paths.test].concat(paths.bowerSrc))
    .pipe(purescript.pscMake({}))
    .pipe(gulp.dest(paths.dest));
});

gulp.task('test', ['test-make'], function() {
  var env = {};
  for (var e in process.env) env[e] = process.env[e];
  env.NODE_PATH = 'output';
  run('node -e "require(\'Test.Main\').main();"', { 'env': env }).exec();
});

gulp.task('example-make', function() {
  return gulp.src([paths.src, paths.example].concat(paths.bowerSrc))
    .pipe(purescript.pscMake({}))
    .pipe(gulp.dest(paths.dest));
});

gulp.task('example', ['example-make'], function() {
  var nodePath = process.env.NODE_PATH;
  var buildPath = path.resolve(paths.dest);
  process.env["NODE_PATH"] = nodePath ? (buildPath + ":" + nodePath) : buildPath;
  return browserify({
    entries: "../examples/example.src.js",
    basedir: buildPath
  }).bundle()
    .pipe(source('example.js'))
    .pipe(gulp.dest('./examples/'));
});

gulp.task('docs', function() {
  return gulp.src(paths.docSrc)
    .pipe(purescript.pscDocs())
    .pipe(gulp.dest(paths.doc));
});

gulp.task('psci', function() {
  return gulp.src([paths.src, paths.test, paths.example].concat(paths.bowerSrc))
    .pipe(purescript.dotPsci());
});

gulp.task('watch', function() {
  gulp.watch(paths.src, ['make', 'docs']);
});

gulp.task('default', ['make', 'docs']);
