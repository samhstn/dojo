const { log } = require('./helpers.js')

function slidePuzzle(arr) {
  const len = arr.length
  const slides = []
  const push = push_(arr, slides)
  let n = flat(arr).findIndex((n, i) => n != i + 1) + 1

  while (n !== len ** 2) {
    if (n > (len - 2) * len) {
      lastTwoRows(arr, n, push)
    } else if ((n - 1) % len > len - 3) {
      lastTwoColumns(arr, n, push)
    } else {
      place(arr, n, push)
    }
    n = flat(arr).findIndex((n, i) => n !== i + 1) + 1
  }
  return slides
}

function place(arr, n, push) {
  const len = arr.length
  while (flat(arr).indexOf(n) >= len * (Math.floor((n - 1) / len) + 1)) {
  }
}

function flat(arr) {
  return arr.reduce((a, c) => a.concat(c), [])
}

function push_(arr, slides) {
  function f(n) {
    if (Array.isArray(n)) {
      return n.forEach(f)
    }
    // console.log('PUSHING', n)
    const nRow = arr.findIndex(r => r.includes(n))
    const zRow = arr.findIndex(r => r.includes(0))
    if (nRow === zRow) {
      const newRow = arr[nRow].map(c => c === 0 ? n : c === n ? 0 : c)
      arr[nRow].splice(0, arr.length, ...newRow)
    } else {
      const col = arr[nRow].indexOf(n)
      const firstRow = Math.min(nRow, zRow)
      const firstVal = arr[firstRow][col]
      arr[firstRow][col] = arr[firstRow + 1][col]
      arr[firstRow + 1][col] = firstVal
    }
    slides.push(n)
  }

  return f
}

function placeZeroAt(arr, index) {
  return arr.indexOf(0) > index
    ? arr.slice(index, arr.indexOf(0)).reverse()
    : arr.slice(arr.indexOf(0) + 1, index + 1)
}

const test = require('tape')

test('')
