#!/usr/bin/env node

// script is used to generate json which is used in our elm tests

function weekdayToString(n) {
  return ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'][n];
}

function generateJson(n) {
  var min = new Date('1 Jan 1800').getTime();
  var max = new Date('1 Jan 2100').getTime();

  var i = 0;
  var list = [];
  var millis, date, day, weekday, month, year, name;

  while (i < n) {
    millis = Math.floor(min + (Math.random() * (max - min)));
    date = new Date(millis);
    day = date.getDate();
    weekday = date.getDay();
    month = date.getMonth();
    year = 1900 + date.getYear();
    name = `${weekdayToString(weekday)}, ${day}/${month + 1}/${year}`;

    list.push({ millis, date, day, weekday, month, year, name });
    i++;
  }

  console.log(JSON.stringify(list));
}

var iterations = parseInt(process.argv[2], 10);

if (isNaN(iterations)) {
  throw Error('Expected integer argument');
}

generateJson(iterations);
