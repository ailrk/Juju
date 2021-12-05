'use strict';

var gulp = require('gulp'),
  http = require('http'),
  st = require('st'),
  exec = require('child_process').exec,
  gutil = require('gulp-util'),
  clear = require('clear'),
  counter = 0;

var cmd = 'elm make ./src/Main.elm --output ./dist/bundle.js';
clear();

gulp.task('server', function(done) {
  console.log('Starting server at http://localhost:4000');
  http.createServer(
    st({
      path: __dirname,
      index: 'index.html',
      cache: false
    })
  ).listen(4000, done);
});

gulp.task('elm', function(cb) {
  if (counter > 0) {
    clear();
  }
  exec(cmd, function(err, stdout, stderr) {
    if (err) {
      console.log('elm make: ', stderr);
    } else {
      console.log('elm make: ', stdout);
    }
    cb();
  });
  counter++;
});

gulp.task('watch', function(cb) {
  gulp.watch('**/**/*.elm', gulp.series('elm'));
});


gulp.task('default', gulp.series('server', 'watch', 'elm'));
