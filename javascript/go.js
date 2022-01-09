// https://www.codewars.com/kata/59de9f8ff703c4891900005c

const test = require('tape')

const alphabet = 'ABCDEFGHJKLMNOPQRSTUVWXYZ'

class Go {
  constructor(size) {
    if (size > 25) {
      throw 'Board cannot be larger than 25 by 25.'
    }
    this.alphabet = alphabet.slice(0, size)
    this.size = size
    this.prevBoard = []
    this.board = '.'.repeat(size).split('').map(_ => '.'.repeat(size).split(''))
    this.blackToMove = true
  }

  log() {
    const size = this.size
    const top = this.alphabet.split('')
    console.log(
      [top].concat(this.board)
           .map((row, i) => `${i === 0 ? ' ' : size - i + 1} ${row.join(' ')}`)
           .join('\n')
    )
  }

  parseCoord(coord) {
    return [parseInt(coord.slice(0, -1)), coord.slice(-1)]
  }

  neighbours(coord) {
    const [n, c] = this.parseCoord(coord)
    const size = this.size
    return [
      [n + 1, c],
      [n, this.alphabet[this.alphabet.indexOf(c) + 1]],
      [n - 1, c],
      [n, this.alphabet[this.alphabet.indexOf(c) - 1]]
    ].reduce((a, [newN, newC]) => {
      return newN > 0 && newN <= size && newC
        ? a.concat(`${newN}${newC}`)
        : a
    }, [])
  }

  getPosition(coord) {
    const [n, c] = this.parseCoord(coord)
    return this.board[this.size - n][alphabet.indexOf(c)]
  }

  hasLiberties(coord, colour) {
    const visited = []
    const board = this.board
    let n = [coord]

    while (n.length > 0) {
      visited.push(...n)
      const newN = []
      n.forEach(coord => {
        this.neighbours(coord)
          .filter(c => ['.', colour].includes(this.getPosition(c)) && !visited.includes(c))
          .forEach(c => {
            newN.push(c)
          })
      })

      if (newN.some(c => this.getPosition(c) === '.')) {
        return true
      } else if (n.length === 0) {
        return false
      } else {
        n = newN
      }
    }
    return false
  }

  capture(coord) {
    const [n, c] = this.parseCoord(coord)
    const position = this.getPosition(coord)
    if (position !== '.') {
      this.board[this.size - n][alphabet.indexOf(c)] = '.'
      this.neighbours(coord)
        .filter(c => this.getPosition(c) === position)
        .forEach(c => this.capture(c))
    }
  }

  move(...coords) {
    for (const coord of coords) {
      const boardCopy = this.board.map(row => row.slice())
      const [n, c] = this.parseCoord(coord)
      if (this.getPosition(coord) !== '.') {
        throw 'not placeable'
      }
      const [moving, notMoving] = this.blackToMove ? ['x', 'o'] : ['o', 'x']
      this.board[this.size - n][alphabet.indexOf(c)] = moving

      this.neighbours(coord)
        .filter(c => this.getPosition(c) === notMoving)
        .forEach(neighbourCoord => {
          if (!this.hasLiberties(neighbourCoord, notMoving)) {
            this.capture(neighbourCoord)
          }
        })

      if (!this.hasLiberties(coord, moving)) {
        this.board = boardCopy
        throw 'Illegal move'
      }

      if (this.prevBoard === this.board.map(row => row.join('')).join('\n')) {
        this.board = boardCopy
        throw 'KO illegal move'
      }

      this.prevBoard = boardCopy.map(row => row.join('')).join('\n')
      this.blackToMove = !this.blackToMove
    }
  }

  handicapStones(handicapN) {
    if (this.board.some(row => row.some(c => c !== '.'))) {
      throw 'game shouldn\'t have started'
    }

    if (this.size === 9) {
      if (handicapN < 1 || handicapN > 5) {
        throw 'invalid number for handicap stones'
      }
      ['7G', '3C', '3G', '7C', '5E'].slice(0, handicapN).forEach(coord => {
        const [n, c] = this.parseCoord(coord)
        this.board[this.size - n][alphabet.indexOf(c)] = 'x'
      })
    } else if (this.size === 13) {
      if (handicapN < 1 || handicapN > 9) {
        throw 'invalid number for handicap stones'
      }
      ['10K', '4D', '4K', '10D', '7G', '7D', '7K', '7G', '4G'].slice(0, handicapN).forEach(coord => {
        const [n, c] = this.parseCoord(coord)
        this.board[this.size - n][alphabet.indexOf(c)] = 'x'
      })
    } else if (this.size === 19) {
      if (handicapN < 1 || handicapN > 9) {
        throw 'invalid number for handicap stones'
      }
      ['16Q', '4D', '4Q', '16D', '10K', '10D', '10Q', '16K', '4K'].slice(0, handicapN).forEach(coord => {
        const [n, c] = this.parseCoord(coord)
        this.board[this.size - n][alphabet.indexOf(c)] = 'x'
      })
    } else {
      throw 'invalid board size for handicap stones'
    }
  }
}

test("Creating go boards", t => {
  let board = [[".",".",".",".",".",".",".",".","."],
               [".",".",".",".",".",".",".",".","."],
               [".",".",".",".",".",".",".",".","."],
               [".",".",".",".",".",".",".",".","."],
               [".",".",".",".",".",".",".",".","."],
               [".",".",".",".",".",".",".",".","."],
               [".",".",".",".",".",".",".",".","."],
               [".",".",".",".",".",".",".",".","."],
               [".",".",".",".",".",".",".",".","."]]
                
  let game = new Go(9)
  t.deepEqual(game.board, board)

  board = [[".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".","."]]
  
  game = new Go(13)
  t.deepEqual(game.board, board)

  board = [[".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."],
           [".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".",".","."]]
  
  game = new Go(19)
  t.deepEqual(game.board, board)

  t.throws(() => new Go(32), 'game.create(32) should throw an error. Board cannot be larger than 25 by 25.')

  t.end()
})

test("Placing stones", t => {
  let game = new Go(9)
  t.equal(game.getPosition('3D'), '.')
  game.move('3D');
  t.equal(game.getPosition('3D'), 'x')

  t.equal(game.getPosition('4D'), '.')
  game.move('4D');
  t.equal(game.getPosition('4D'), 'o')

  game = new Go(9)
  t.equal(game.getPosition('4A'), '.')
  t.equal(game.getPosition('5A'), '.')
  t.equal(game.getPosition('6A'), '.')
  game.move('4A','5A','6A')
  t.equal(game.getPosition('4A'), 'x')
  t.equal(game.getPosition('5A'), 'o')
  t.equal(game.getPosition('6A'), 'x')

  t.throws(() => game.move('4A'), 'err')

  t.throws(() => game.move('3Z'), /not placeable/)
  t.throws(() => game.move('66'))

  t.end()
})

test('Neighbours', t => {
  let game = new Go(9)
  t.deepEqual(game.neighbours('4D'), ['5D', '4E', '3D', '4C'])
  t.deepEqual(game.neighbours('1C'), ['2C', '1D', '1B'])
  t.deepEqual(game.neighbours('3A'), ['4A', '3B', '2A'])
  t.deepEqual(game.neighbours('9C'), ['9D', '8C', '9B'])
  t.deepEqual(game.neighbours('8J'), ['9J', '7J', '8H'])
  t.deepEqual(game.neighbours('1A'), ['2A', '1B'])
  t.deepEqual(game.neighbours('9A'), ['9B', '8A'])
  t.deepEqual(game.neighbours('9J'), ['8J', '9H'])
  t.deepEqual(game.neighbours('1J'), ['2J', '1H'])
  t.end()
})

test('hasLiberties', t => {
  let game = new Go(9)
  game.board = [
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', 'x', '.', 'o', '.', '.', '.', '.', '.'],
    ['.', '.', 'o', 'x', 'o', '.', '.', 'x', '.'],
    ['.', '.', '.', 'o', '.', '.', '.', 'x', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.']
  ]
  t.equal(game.hasLiberties('4D', 'x'), false)
  t.equal(game.hasLiberties('4C', 'o'), true)

  game.board = [
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', 'x', 'o', 'o', '.', '.', '.', '.', '.'],
    ['.', 'o', 'x', 'x', 'o', '.', '.', 'x', '.'],
    ['.', '.', '.', 'o', '.', '.', '.', 'x', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.']
  ]
  t.equal(game.hasLiberties('4D', 'x'), true)
  game.board = [
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', 'x', 'o', 'o', '.', '.', '.', '.', '.'],
    ['.', 'o', 'x', 'x', 'o', '.', '.', 'x', '.'],
    ['.', '.', 'o', 'o', '.', '.', '.', 'x', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.']
  ]
  t.equal(game.hasLiberties('4D', 'x'), false)
  t.end()
})

test('capture', t => {
  let game = new Go(9)
  game.board = [
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', 'x', '.', 'o', '.', '.', '.', '.', '.'],
    ['.', '.', 'o', 'x', 'o', '.', '.', 'x', '.'],
    ['.', '.', '.', 'o', '.', '.', '.', 'x', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.']
  ]
  game.capture('4D')
  t.equal(game.getPosition('4D'), '.')

  game.board = [
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', 'x', 'o', 'o', '.', '.', '.', '.', '.'],
    ['.', 'o', 'x', 'x', 'o', '.', '.', 'x', '.'],
    ['.', '.', 'o', 'o', '.', '.', '.', 'x', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.']
  ]
  const newBoard = [
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', 'x', 'o', 'o', '.', '.', '.', '.', '.'],
    ['.', 'o', '.', '.', 'o', '.', '.', 'x', '.'],
    ['.', '.', 'o', 'o', '.', '.', '.', 'x', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.'],
    ['.', '.', '.', '.', '.', '.', '.', '.', '.']
  ]
  game.capture('4D')
  t.deepEqual(game.board, newBoard)
  t.end()
})

test('Capturing :: one', t => {
  const game = new Go(9)
  const moves = ["4D","3D","4H","5D","3H","4C","5B","4E"]
  game.move(...moves)
  t.equal(game.getPosition('4D'), '.')
  t.end()
})

test('Capturing :: multiple', t => {
  const game = new Go(9)
  const moves = ["6D","7E","6E","6F","4D","5E","5D","7D",
                 "5C","6C","7H","3D","4E","4F","3E","2E",
                 "3F","3G","2F","1F","2G","2H","1G","1H",
                 "4C","3C","6H","4B","5H","5B"]
  const captured = ["6D", "6E", "4D", "5D", "5C", "4E", "3E","3F","2F","2G","1G","4C"]
  game.move(...moves);
  t.plan(captured.length)
  captured.forEach(capture => {
    t.equal(game.getPosition(capture), ".")
  })
  t.end()
})

test('Capturing :: corner', t => {
  const game = new Go(9)
  const moves = ["9A","8A","8B","9B"]
  game.move(...moves)
  t.equal(game.getPosition('9A'), ".")
  t.end()
})

test('Capturing :: multiple2', t => {
  const game = new Go(9)
  const moves = ["5D","5E","4E","6E","7D","4F","7E","3E","5F","4D",
                 "6F","6D","6C","7F","4E","5E"]
  const captured = ["4E","6D","6E"]
  game.move(...moves)
  t.plan(captured.length)
  captured.forEach(capture => {
    t.equal(game.getPosition(capture),".")
  })
  t.end()
})

test('Capturing :: snapback', t => {
  const game = new Go(9)
  const moves = ["5A","1E","5B","2D","5C","2C","3A",
                 "1C","2A","3D","2B","3E","4D","4B",
                 "4E","4A","3C","3B","1A","4C","3C"]
  const captured = ["4A","4B","4C","3B"]
  game.move(...moves)
  t.plan(captured.length)
  captured.forEach(capture => {
    t.equal(game.getPosition(capture), ".")
  })
  t.end()
})

test('Capturing :: self throws an error', t => {
  const game = new Go(9)
  const moves = ["4H","8A","8B","9B","9A"]
  t.throws(() => game.move(...moves))
  t.equal(game.getPosition('9A'), ".")
  game.move("3B")
  t.equal(game.getPosition("3B"), "x", "Black should have another try.")
  t.end()
})

test('KO rule', t => {
  const game = new Go(5)
  const moves = ["5C","5B","4D","4A","3C","3B",
                 "2D","2C","4B","4C","4B"]
  t.throws(() => game.move(...moves))
  game.move("2B");
  t.equal(game.getPosition("2B"), "x","Black should be given another try to place their stone.");
  t.equal(game.getPosition("4B"), ".","Should rollback game to before illegal move was made.");
  t.end()
})

test('Handicap stones', t => {
  const game = new Go(9)
  const finalBoard = [[ '.', '.', '.', '.', '.', '.', '.', '.', '.' ],
                      [ '.', '.', '.', '.', '.', '.', '.', '.', '.' ],
                      [ '.', '.', '.', '.', '.', '.', 'x', '.', '.' ],
                      [ '.', '.', '.', '.', '.', '.', '.', '.', '.' ],
                      [ '.', '.', '.', '.', '.', '.', '.', '.', '.' ],
                      [ '.', '.', '.', '.', '.', '.', '.', '.', '.' ],
                      [ '.', '.', 'x', '.', '.', '.', 'x', '.', '.' ],
                      [ '.', '.', '.', '.', '.', '.', '.', '.', '.' ],
                      [ '.', '.', '.', '.', '.', '.', '.', '.', '.' ]]
                    
  game.handicapStones(3)
  t.deepEqual(game.board, finalBoard)
  t.end()
})

// describe("Misc", function() {
//   let game = new Go(9,16);
//   it("Can get board size", function() {
//     Test.assertDeepEquals(game.size, {height: 9, width: 16});
//   })
//   it("Can get color of current turn", function() {
//     let game = new Go(9);
//     game.move("3B");
//     Test.assertEquals(game.turn, "white");
//     game.move("4B");
//     Test.assertEquals(game.turn, "black");
//   })
//   it("Can rollback a set number of turns", function() {
//     let game = new Go(9);
//     let board = [[".",".",".",".",".",".",".",".","."],
//                  [".",".",".",".",".",".",".",".","."],
//                  [".",".",".",".",".",".",".",".","."],
//                  [".",".",".",".",".",".",".",".","."],
//                  [".",".",".",".",".",".",".",".","."],
//                  [".",".",".",".",".",".",".",".","."],
//                  [".",".",".",".",".",".",".",".","."],
//                  [".",".",".",".",".",".",".",".","."],
//                  [".",".",".",".",".",".",".",".","."]]
//     game.move("3B","2B","1B");
//     game.rollback(3);
//     Test.assertDeepEquals(game.board,board);
//   })
//   it("Can pass turn", function() {
//     let game = new Go(9);
//     game.pass();
//     Test.assertEquals(game.turn, "white");
//   })
//   it("Can reset the board", function() {
//     let game = new Go(9);
//     let board = [[".",".",".",".",".",".",".",".","."],
//                  [".",".",".",".",".",".",".",".","."],
//                  [".",".",".",".",".",".",".",".","."],
//                  [".",".",".",".",".",".",".",".","."],
//                  [".",".",".",".",".",".",".",".","."],
//                  [".",".",".",".",".",".",".",".","."],
//                  [".",".",".",".",".",".",".",".","."],
//                  [".",".",".",".",".",".",".",".","."],
//                  [".",".",".",".",".",".",".",".","."]]
//     
//     game.move("3B","2B","1B");
//     game.reset();
//     Test.assertDeepEquals(game.board,board);
//     Test.assertEquals(game.turn, "black");
//   })
// })
