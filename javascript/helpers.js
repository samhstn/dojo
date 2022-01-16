const test = require('tape')

test.skip('validateSolution', t => {
  const puzzle = [
    [ 1, 2, 3, 4],
    [ 5, 0, 6, 8],
    [ 9,10, 7,11],
    [13,14,15,12]
  ]
  t.notOk(validateSolution(puzzle, [6,7,11]))
  t.ok(validateSolution(puzzle, [6,7,11,12]))
  t.end()
})

function validateSolution(puzzle, slides) {
  puzzle = puzzle.slice()
  while (slides.length > 0) {
    const move = slides.shift()
    OUTER: for (let j = 0; j < puzzle.length; j++) {
      for (let i = 0; i < puzzle.length; i++) {
        if ([0, move].includes(puzzle[j][i])) {
          if ([0, move].includes(puzzle[j][i + 1])) {
            const newRow = puzzle[j].map((c, idx) => {
              return idx === i ? puzzle[j][i + 1]
                : idx === i + 1 ? puzzle[j][i]
                : c
            })
            puzzle[j] = newRow
          } else {
            const newTopRow = puzzle[j].map((c, idx) => {
              return i === idx ? puzzle[j + 1][i] : c
            })
            const newBottomRow = puzzle[j + 1].map((c, idx) => {
              return i === idx ? puzzle[j][i] : c
            })
            puzzle[j] = newTopRow
            puzzle[j + 1] = newBottomRow
          }
          break OUTER
        }
      }
    }
  }

  return puzzle.every((r, ri) => {
    return r.every((c, ci) => {
      return c === (ri * puzzle.length + ci + 1) % (puzzle.length ** 2)
    })
  })
}

function log(puzzle, slides) {
  if (slides) {
    console.log('SLIDES: ', slides)
  }
  console.log(
    puzzle.map(r =>
      r.map(c => `${c > 9 ? '' : ' '}${c}`).join(' ')).join('\n')
  )
}

module.exports = {
  validateSolution,
  log
}
