const { log } = require('./helpers.js')
const test = require('tape')

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
  let nRowIndex = arr.findIndex(r => r.includes(n))
  const nColIndex = flat(arr).indexOf(n) % len
  if (nRowIndex === len - 1) {
    push(zeroToRow(arr, nRowIndex - 1))
    push(zeroToCol(arr, nColIndex))
    push(n)
    nRowIndex -= 1
  } else {
    push(zeroToRow(arr, nRowIndex + 1))
    push(zeroToCol(arr, nColIndex))
  }
  const moves = ((n - 1) % len) - (flat(arr).indexOf(n) % len)
  push(shuffleHorizontal(arr, n, moves))
  const up = nRowIndex - Math.floor((n - 1) / len)
  shuffleUp(arr, n, up, push)
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

function zeroToCol(arr, index) {
  const zRow = arr.find(r => r.includes(0))
  return placeZeroAt(zRow, index)
}

function zeroToRow(arr, index) {
  const zCol = arr.map(r => r[flat(arr).indexOf(0) % arr.length])
  return placeZeroAt(zCol, index)
}

test('zeroToCol and zeroToRow', t => {
  const puzzle = [
    [21, 2, 3, 4, 5],
    [ 6, 7, 8, 9,10],
    [11,12,13,14,15],
    [16,17,18,19,20],
    [ 1,22, 0,23,24]
  ]
  t.deepEqual(zeroToCol(puzzle, 1), [22])
  t.deepEqual(zeroToCol(puzzle, 4), [23, 24])
  t.deepEqual(zeroToCol(puzzle, 2), [])

  t.deepEqual(zeroToRow(puzzle, 0), [18, 13, 8, 3])
  t.deepEqual(zeroToRow(puzzle, 4), [])

  t.end()
})

function shuffleHorizontal(arr, n, moves) {
  const nRow = arr.find(r => r.includes(n))
  const zRow = arr.find(r => r.includes(0))
  const zColIndex = zRow.indexOf(0)
  const shuffle = []
  const next = moves > 0 ? 1 : -1
  for (let i = zColIndex; i !== zColIndex + moves; i += next) {
    shuffle.push(
      zRow[i + next],
      nRow[i + next],
      n,
      zRow[i + next],
      nRow[i + next]
    )
  }
  return shuffle
}

test('shuffleHorizontal', t => {
  const puzzle = [
    [21, 2, 3, 4, 5],
    [ 6, 7, 8, 9,10],
    [11,12,13,14,15],
    [16,17, 1,19,20],
    [18,22, 0,23,24]
  ]
  t.deepEqual(shuffleHorizontal(puzzle, 1, -2),
    [22, 17, 1, 22, 17, 18, 16, 1, 18, 16])
  t.deepEqual(shuffleHorizontal(puzzle, 1, 2),
    [23, 19, 1, 23, 19, 24, 20, 1, 24, 20])
  t.deepEqual(shuffleHorizontal(puzzle, 1, 0), [])
  t.end()
})

function shuffleUp(arr, n, moves, push) {
  const zRowIndex = arr.findIndex(r => r.includes(0))
  for (let i = zRowIndex; i > zRowIndex - moves; i--) {
    const nCol = arr.map(r => r[flat(arr).indexOf(n) % arr.length])
    const npoCol = arr.map(r => r[(flat(arr).indexOf(n) + 1) % arr.length])
    push([
      npoCol[i],
      npoCol[i - 1],
      npoCol[i - 2],
      nCol[i - 2],
      n
    ])
  }
}

test('shuffleUp', t => {
  const puzzle = [
    [21, 2, 3, 4, 5],
    [ 6, 7, 8, 9,10],
    [11,12, 1,14,15],
    [16,17, 0,19,20],
    [18,22,13,23,24]
  ]
  const slides = []
  const push = push_(puzzle, slides)
  shuffleUp(puzzle, 1, 2, push)
  t.deepEqual(slides, [19, 14, 9, 8, 1, 9, 8, 4, 3, 1])
  t.end()
})

test('place', t => {
  const puzzle = [
    [21,24,16, 4, 5],
    [ 6, 7, 8, 9,10],
    [11,12,13,14,15],
    [ 3,17,18,19,20],
    [ 1,22, 0,23, 2]
  ]
  const slides = []
  const push = push_(puzzle, slides)

  place(puzzle, 1, push)
  t.equal(puzzle[0][0], 1)
  t.equal(puzzle[1][0], 0)

  place(puzzle, 2, push)
  t.deepEqual(puzzle[0].slice(0, 2), [1, 2])
  t.equal(puzzle[1][1], 0)

  place(puzzle, 3, push)
  t.deepEqual(puzzle[0].slice(0, 3), [1, 2, 3])
  t.equal(puzzle[1][2], 0)

  t.end()
})

test('lastTwoColumns', t => {
  t.end()
})
