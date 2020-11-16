let matrix = (m, n, headers) => {
  let data = new Array(m);
  for (let i = 0; i < data.length; i++) {
    data[i] = new Array(n).fill(0);
  }

  // takes in 2 matrices
  multiply = (a, b) => {
    let result = new Matrix(a.m, b.n);
    for (let row = 0; row < result.m; row++) {
      for (let col = 0; col < result.n; col++) {
        let dotProduct = 0;
        for (let term = 0; term < a.n; term++) {
          dotProduct += a.data[row][term] * b.data[term][col];
        }
        result.data[row][col] = dotProduct;
      }
    }
    return result;
  };
  swapRows = (a, b) => {
    let temp = data[a];
    data[a] = data[b];
    data[b] = temp;
  };
  swapCols = (a, b) => {
    if (a == n - 1 || b == n - 1) {
      throw new Error("cannot swap constants row");
    }
    for (let i in data) {
      let myData = data[i];
      let temp = myData[a];
      myData[a] = myData[b];
      myData[b] = temp;
    }
    let temp = headers[a];
    headers[a] = headers[b];
    headers[b] = temp;
  };
  scaleBy = (row, alpha) => {
    if (alpha == 0) {
      throw new Error("Losing data");
    }
    for (let i in data[row]) {
      data[row][i] *= alpha;
    }
  };
  addTo = (source, alpha, target) => {
    if (source == target && alpha == -1) {
      throw new Error("Losing data");
    }
    for (let i = 0; i < n; i++) {
      data[target][i] += alpha * data[source][i];
    }
  };
  toRREF = () => {
    // for each row
    // s is the row and col of the current element that should be turned in a leading one with only zeros in the rest of its coloumn
    if (m > n) {
      throw new Error("too many rows??"); // TODO: make too many rows safe -> deal with duplicate rows...
    }
    for (let s = 0; s < m; s++) {
      // get the current col to be nonzero by swapping cols to the right
      for (let col = s; data[s][s] === 0; col++) {
        swapCols(s, col); // the first swap is pointless... swapCols(0,0)
        if (col + 1 >= n - 1) {
          if (data[s][n - 1] === 0) {
            // the current row is all zeros
            // TODO: goto swapCols "for(let col..." for the same row (repeat this row)
          } else {
            // all coefficients are zero, but the constant isn't
            throw new Error("there is no solution to this matrix");
          }
        }
      }
      // the current row should now be "in REF"
      // divide current col to make leading nonzero a one
      scaleBy(s, 1 / data[s][s]);
      // subtract current row from all others to make all other elements in col of the current element zero
      for (let row in data) {
        if (row != s) {
          // why is row a string?
          addTo(s, -data[row][s], row);
        }
      }
    }
    return this;
  };
  deleteAllZeroCols = () => {
    for (let col = 0; col < n; col++) {
      let hasNonZero = false;
      for (let row in data) {
        if (data[row][col] !== 0) {
          hasNonZero = true;
          break;
        }
      }
      if (!hasNonZero) {
        // remove coloumn
        for (let row in data) {
          data.splice(col, 1);
        }
      }
    }
  };
  deleteAllZeroRows = () => {
    for (let row in data) {
      let hasNonZero = false;
      for (let col in data[row]) {
        if (data[row][col] != 0) {
          hasNonZero = true;
          break;
        }
      }
      if (!hasNonZero) {
        // remove row
        data.splice(row, 1);
      }
    }
  };

  parseString = (str) => {
    str += "; "; // ensure the last equation is parsed normally
    let strs = [];
    // first divide into equations
    let index = str.search(";");
    while (index !== -1) {
      strs.push(str.slice(0, index));
      str = str.slice(index + 1); // +1 for the char ';'
      index = str.search(";");
    }

    // find variable names
    let varNamesArray = [];
    for (let i in strs) {
      let myStr = strs[i];
      for (let j = 0; j < myStr.length; j++) {
        let currentVarName = [];
        let myASCII = myStr.charCodeAt(j);
        while (
          (65 <= myASCII && myASCII <= 90) ||
          (97 <= myASCII && myASCII <= 122)
        ) {
          currentVarName.push(myStr[j]);
          j++;
          myASCII = myStr.charCodeAt(j);
        }
        varNamesArray.push(currentVarName);
      }
    }

    // remove duplicate variable names and make list of strings, not a list of lists of characters
    let varNames = [];
    varNamesArray.forEach((varNameArray) => {
      let varName = varNameArray.reduce((acc, s) => acc + s, "");
      if (!varNames.includes(varName) && varName !== "") {
        varNames.push(varName);
      }
    });

    // create matrix with dimensions
    let matrix = new Matrix(strs.length, varNames.length + 1, varNames);

    // find and insert values
    let row = 0;
    for (let i in strs) {
      let str = strs[i];
      str += " "; // to ensure that the last term is parsed normally
      let buffer = []; // holds the latest characters
      let bufferType = "";
      let coefficient = 0;
      let sign = 1; // holds 1 or -1
      for (let i in str) {
        let char = str[i];
        let charCode = char.charCodeAt(0);
        if (45 <= charCode && charCode <= 57 && charCode !== 47) {
          // the char is a number
          if (bufferType === "letter") {
            throw new Error("num right after letter");
          }
          buffer.push(char);
          bufferType = "num";
        } else if (
          (97 <= charCode && charCode <= 122) ||
          (65 <= charCode && charCode <= 90)
        ) {
          // the char is letter
          if (bufferType === "num") {
            // flush the buffer into coefficient
            coefficient =
              sign * parseFloat(buffer.reduce((acc, s) => acc + s, ""));
            buffer = [];
          } else if (bufferType === "") {
            coefficient = sign * 1;
          }
          buffer.push(char);
          bufferType = "letter";
        } else {
          // the char is a symbol (whitespace)
          if (bufferType === "num") {
            // add the constant
            matrix.data[row][matrix.n - 1] +=
              -sign * parseFloat(buffer.reduce((acc, s) => acc + s, ""));
          } else if (bufferType === "letter") {
            // add the coefficent
            let col = matrix.headers.indexOf(
              buffer.reduce((acc, s) => acc + s, "")
            );
            matrix.data[row][col] += coefficient;
            coefficient = 0;
          }
          buffer = [];
          bufferType = "";

          if (char === "=") {
            sign = -1;
          }
        }
      }
      row += 1;
    }
    return matrix;
  };

  // TODO: parameterize independent variables and print all variables in terms of the parameterized variables
  interpretMatrix = () => {
    let output = "";
    for (let rowIndex in data) {
      let row = data[rowIndex];
      let outputForCurrentRow = "";
      for (let colIndex in data[rowIndex]) {
        let val = data[rowIndex][colIndex];
        if (colIndex < n - 1) {
          // if not a constant
          if (val !== 0) {
            // only print a variable if the coefficient is non-zero
            if (outputForCurrentRow !== "") {
              outputForCurrentRow += " + ";
            }
            let varName = headers[colIndex];
            outputForCurrentRow +=
              (val !== 1 ? val.toString() : "") + varName + " ";
          }
        } else {
          outputForCurrentRow += "= " + val.toString();
        }
      }
      output += outputForCurrentRow + "; ";
    }
    return output;
  };

  return {
    multiply,
    swapRows,
    swapCols,
    scaleBy,
    addTo,
    toRREF,
    deleteAllZeroCols,
    deleteAllZeroRows,
    parseString,
    interpretMatrix,
  };
};
