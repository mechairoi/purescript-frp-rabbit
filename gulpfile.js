'use strict';

var gulp        = require('gulp')
  , purescript  = require('gulp-purescript')
  , source      = require('vinyl-source-stream')
  , browserify  = require('browserify')
  , transform   = require('vinyl-transform')
  , path        = require("path")
  ;

var browserified = transform(function(filename){
  return browserify(filename).bundle();
});

var paths = {
  src: 'src/**/*.purs',
  doc: 'MODULE.md',
  bowerSrc: 'bower_components/purescript-*/src/**/*.purs',
  dest: 'output',
  test: 'examples/*.purs'
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
  var nodePath = process.env.NODE_PATH;
  var buildPath = path.resolve(paths.dest);
  process.env["NODE_PATH"] = nodePath ? (buildPath + ":" + nodePath) : buildPath;
  return browserify({
    entries: "../examples/test.js",
    basedir: buildPath
  }).bundle()
    .pipe(source('test.js'))
    .pipe(gulp.dest('./test/'));
});

gulp.task('docs', function() {
  return gulp.src(paths.src)
    .pipe(purescript.pscDocs())
    .pipe(gulp.dest(paths.doc));
});

gulp.task('psci', function() {
  return gulp.src([paths.src, paths.test].concat(paths.bowerSrc))
    .pipe(purescript.dotPsci());
});

gulp.task('watch', function() {
  gulp.watch(paths.src, ['make', 'docs']);
});

gulp.task('default', ['make', 'docs']);
