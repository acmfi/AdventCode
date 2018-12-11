class Element {
  constructor (data) {
    this.val = data;
    this.nxt = null;
    this.prv = null;
  }

  value () {
    return this.val;
  }

  next () {
    return this.nxt;
  }

  prev () {
    return this.prv;
  }

  setNext (elem) {
    this.nxt = elem;
  }

  setPrev (elem) {
    this.prv = elem;
  }
}

class LinkedList {
  constructor (node) {
    this.first = node;
    this.last = node;
  }

  pop () {
    const last = this.last;
    this.last = this.last.prev();
    this.last.setNext(null);
    return last;
  }

  push (node) {
    node.setPrev(this.last);
    this.last.setNext(node);
    this.last = node;
  }

  rol (n=1) {
    this.last.setNext(this.first);
    this.first.setPrev(this.last);
    let newFirst = this.first;
    while(n-- != 0) {
      newFirst = newFirst.next();
    }
    this.last = newFirst.prev();
    this.last.setNext(null);
    this.first = newFirst;
    this.first .setPrev(null);
  }

  ror (n=1) {
    this.last.setNext(this.first);
    this.first.setPrev(this.last);
    let newFirst = this.last;
    while(--n != 0) {
      newFirst = newFirst.prev();
    }
    this.last = newFirst.prev();
    this.last.setNext(null);
    this.first = newFirst;
    this.first .setPrev(null);
  }

  toString() {
    let str = '';
    let current = this.first;
    while (current != null) {
      str += current.value() + ', ';
      current = current.next();
    }
    return '[' + str.replace(/, $/, '') + ']';
  }

  toArray() {
    let arr = new Array();
    let current = this.first;
    while (current != null) {
      arr.push(current.value());
      current = current.next();
    }
    return arr;
  }
}

module.exports = { LinkedList, Element }
