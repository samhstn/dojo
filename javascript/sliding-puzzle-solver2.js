const test = require('tape')
const { validateSolution, log } = require('./helpers.js')

function slidePuzzle(arr) {
  const len = arr.length
  const slides = []
  const push = push_(arr, slides)
  let n = flat(arr).findIndex((n, i) => n !== i + 1) + 1

  while (n !== len ** 2) {
    log(arr)
    console.log('N', n)
    if (n > (len - 2) * len) {
      // last 2 rows
      console.log('last 2 rows')
      const m = n + len
      break
    } else if ((n - 1) % len > len - 3) {
      // last 2 columns
      console.log('last 2 columns')
      const m = n + 1
      // lastTwoColumns(arr, n, push)
      break
    } else {
      console.log('else')
      while (flat(arr)[n - 1] !== flat(arr).indexOf(n)) {
        if (flat(arr).indexOf(n) < len * (Math.floor((n - 1) / len) + 1)) {
          correctRow(arr, n, push)
        } else {
          bringToCorrectRow(arr, n, push)
        }
      }
      break
    }
    n = flat(arr).findIndex((n, i) => n !== i + 1) + 1
  }
  return slides
}

// if 0 position < 1 position, bring to 1 then 2nd row
// bring to row: nRow + 1, then bring to col: pos 1 + len - 1
// then shuffle pattern until finished
function correctRow(arr, n, push) {
  const len = arr.length
  if (flat(arr).indexOf(0) < flat(arr).indexOf(n)) {
    push(
      flat(arr).slice(
        flat(arr).indexOf(0) + 1,
        flat(arr).indexOf(n) + 1
      )
    )
    if (flat(arr)[n - 1] === n) {
      return
    }
    push(flat(arr)[flat(arr).indexOf(n) + 1 + len])
  }
  const zCol = arr.map(r => r[flat(arr).indexOf(0) % len])
  if (zCol[0] === 0) {
    push(zCol[1])
  } else {
    push(zCol.slice(1, zCol.indexOf(0)).reverse())
  }
  const nColIndex = flat(arr).indexOf(n) % len
  const zRow = arr.find(r => r.includes(0))
  push(placeZeroAt(zRow, nColIndex - 1))
  while (flat(arr)[n] !== n) {
    push(flat(arr)[flat(arr).indexOf(n) - 1])
    push(n)
    push(flat(arr)[flat(arr).indexOf(n) + len + 1])
    push(flat(arr)[flat(arr).indexOf(n) + len])
    push(flat(arr)[flat(arr).indexOf(n) + len - 1])
  }
  push(flat(arr)[flat(arr).indexOf(n) - 1])
  push(n)
}

// TODO don't knock out placed numbers
function bringToCorrectRow(arr, n, push) {
  const len = arr.length
  const zCol = arr.map(r => r[flat(arr).indexOf(0) % len])
  push(placeZeroAt(zCol, arr.findIndex(r => r.includes(n)) - 1))
  const zRow = arr.find(r => r.includes(0))
  const nColIndex = flat(arr).indexOf(n) % len
  push(placeZeroAt(zRow, nColIndex))
  push(n)
  while (flat(arr).indexOf(n) >= len * (Math.floor((n - 1) / len) + 1)) {
    push(flat(arr)[flat(arr).indexOf(n) + len + 1])
    push(flat(arr)[flat(arr).indexOf(n) + 1])
    push(flat(arr)[flat(arr).indexOf(n) - len + 1])
    push(flat(arr)[flat(arr).indexOf(n) - len])
    push(n)
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
  if (arr.indexOf(0) > index) {
    return arr.slice(index, arr.indexOf(0)).reverse()
  } else {
    return arr.slice(arr.indexOf(0) + 1, index + 1)
  }
}

test('placeZeroAt', t => {
       // i  0, 1, 2, 3, 4, 5, 6, 7, 8, 9
  const a = [1, 2, 3, 4, 5, 0, 6, 7, 8, 9]
  t.deepEqual(placeZeroAt(a, 0), [5, 4, 3, 2, 1])
  t.deepEqual(placeZeroAt(a, 1), [5, 4, 3, 2])
  t.deepEqual(placeZeroAt(a, 4), [5])
  t.deepEqual(placeZeroAt(a, 5), [])
  t.deepEqual(placeZeroAt(a, 6), [6])
  t.deepEqual(placeZeroAt(a, 8), [6, 7, 8])
  t.end()
})

test('push', t => {
  const puzzle = [
    [4,1,3],
    [2,8,0],
    [7,6,5]
  ]
  const puzzle2 = [
    [4,1,3],
    [2,0,8],
    [7,6,5]
  ]
  const puzzle3 = [
    [4,1,3],
    [2,6,8],
    [7,0,5]
  ]
  const slides = []
  const push = push_(puzzle, slides)
  push(8)
  t.deepEqual(puzzle, puzzle2)
  t.deepEqual(slides, [8])
  push(6)
  t.deepEqual(puzzle, puzzle3)
  t.deepEqual(slides, [8, 6])
  t.end()
})

test('correctRow1', t => {
  const puzzle = [
    [12,10, 3, 1],
    [ 5, 4, 6, 8],
    [ 2,13, 7,15],
    [14, 9, 0,11]
  ]
  const slides = []
  const push = push_(puzzle, slides)
  correctRow(puzzle, 1, push)
  t.deepEqual(slides, [7, 6, 3, 1, 8, 3, 4, 10, 1, 4, 10, 5, 12, 1])
  t.end()
})

test('correctRow2', t => {
  const puzzle = [
    [12, 1, 3,10],
    [ 5, 4, 6, 8],
    [ 2,13, 7,15],
    [14, 9, 0,11]
  ]
  const slides = []
  const push = push_(puzzle, slides)
  correctRow(puzzle, 1, push)
  t.deepEqual(slides, [7, 6, 4, 5, 12, 1])
  t.end()
})

test('correctRow3', t => {
  const puzzle = [
    [ 0, 1, 3,10],
    [ 5, 4, 6, 8],
    [ 2,13, 7,15],
    [14, 9,12,11]
  ]
  const slides = []
  const push = push_(puzzle, slides)
  correctRow(puzzle, 1, push)
  t.deepEqual(slides, [1])
  t.end()
})

test('correctRow4', t => {
  const puzzle = [
    [ 4, 1, 3,10],
    [ 5, 0, 6, 8],
    [ 2,13, 7,15],
    [14, 9,12,11]
  ]
  const slides = []
  const push = push_(puzzle, slides)
  correctRow(puzzle, 1, push)
  t.deepEqual(slides, [5, 4, 1])
  t.end()
})

test('correctRow5', t => {
  const puzzle = [
    [ 5, 1, 3,10],
    [ 0, 4, 6, 8],
    [ 2,13, 7,15],
    [14, 9,12,11]
  ]
  const slides = []
  const push = push_(puzzle, slides)
  correctRow(puzzle, 1, push)
  t.deepEqual(slides, [5, 1])
  t.end()
})

test('correctRow6', t => {
  const puzzle = [
    [12,10, 3, 1],
    [ 5, 4, 6, 8],
    [ 9,13, 7,15],
    [ 0, 2,14,11]
  ]
  const slides = []
  const push = push_(puzzle, slides)
  correctRow(puzzle, 1, push)
  t.deepEqual(slides, [9, 5, 4, 6, 3, 1, 8, 3, 6, 10, 1, 6, 10, 4, 12, 1])
  t.end()
})

test('bringToCorrectRow1', t => {
  const puzzle = [
    [12,10, 3, 4],
    [ 5, 1, 6, 8],
    [ 9,13, 7,15],
    [ 0, 2,14,11]
  ]
  const slides = []
  const push = push_(puzzle, slides)
  bringToCorrectRow(puzzle, 1, push)
  t.deepEqual(slides, [9, 5, 12, 10, 1])
  t.end()
})

test('bringToCorrectRow2', t => {
  const puzzle = [
    [12,10, 3, 4],
    [ 5, 2, 6, 8],
    [ 9,13, 7,15],
    [ 0, 1,14,11]
  ]
  const slides = []
  const push = push_(puzzle, slides)
  bringToCorrectRow(puzzle, 1, push)
  t.deepEqual(slides, [9, 13, 1, 14, 7, 6, 2, 1, 6, 2, 3, 10, 1])
  t.end()
})

test.skip('bringToCorrectRow3', t => {
  const puzzle = [
    [ 1,10, 3, 4],
    [ 5, 2, 6, 8],
    [ 9,13, 7,15],
    [ 0,12,14,11]
  ]
  const slides = []
  const push = push_(puzzle, slides)
  bringToCorrectRow(puzzle, 1, push)
  t.deepEqual(slides, [9, 13, 1, 14, 7, 6, 2, 1, 6, 2, 3, 10, 1])
  t.end()
})

test.skip('lastTwoColumns', t => {
  const puzzle = [
    [ 1, 2,11,12],
    [ 5,10, 6, 8],
    [ 9,13, 7,15],
    [ 0, 4,14, 3]
  ]
  const slides = []
  const push = push_(puzzle, slides)
  bringToCorrectRow(puzzle, 1, push)
  t.deepEqual(slides, [9, 13, 1, 14, 7, 6, 2, 1, 6, 2, 3, 10, 1])
  t.end()
})

test.skip('puzzle1', t => {
  const puzzle = [
    [4,1,3],
    [2,8,0],
    [7,6,5]
  ]
  const slides = slidePuzzle(puzzle) // [8, 2, 4, 1, 2]
  console.log('SLIDES', slides)
  t.ok(validateSolution(puzzle, slides))
  t.end()
})

test.skip('puzzle2', t => {
  const puzzle = [
    [10, 3, 6, 4],
    [ 1, 5, 8, 0],
    [ 2,13, 7,15],
    [14, 9,12,11]
  ]
  t.ok(validateSolution(e, slidePuzzle(puzzle)))
  t.end()
})

test.skip('puzzle3', t => {
  const puzzle1 = [
    [ 3, 7,14,15,10],
    [ 1, 0, 5, 9, 4],
    [16, 2,11,12, 8],
    [17, 6,13,18,20],
    [21,22,23,19,24]
  ]
  t.ok(validateSolution(e, slidePuzzle(puzzle1)))
  t.end()
})
