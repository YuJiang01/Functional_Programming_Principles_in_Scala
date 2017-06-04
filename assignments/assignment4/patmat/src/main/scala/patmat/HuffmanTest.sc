package patmat

package patmat

import common._

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {


    abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree
  

  // Part 1: Basics
    def weight(tree: CodeTree): Int = tree match{
      case Leaf(_,w) => w
      case Fork(_,_,_,w) => w
    }                                             //> weight: (tree: patmat.patmat.Huffman.CodeTree)Int
  
    def chars(tree: CodeTree): List[Char] =  tree match {
      case Leaf(c,_) =>  List(c)
      case Fork(_,_,c,_) => c
    }                                             //> chars: (tree: patmat.patmat.Huffman.CodeTree)List[Char]
  
  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))
                                                  //> makeCodeTree: (left: patmat.patmat.Huffman.CodeTree, right: patmat.patmat.Hu
                                                  //| ffman.CodeTree)patmat.patmat.Huffman.Fork


  def string2Chars(str: String): List[Char] = str.toList
                                                  //> string2Chars: (str: String)List[Char]

    def times(chars: List[Char]): List[(Char, Int)] = {
    	def addTimes(chars2: List[Char], counts: List[(Char,Int)]): List[(Char,Int)] = {
    		if(chars2.isEmpty) counts
    		else if(!counts.exists(x=>x._1==chars2.head))   addTimes(chars2.tail, (chars2.head,1)::counts)
    		else {
    			val currentCount = counts.filter(x=>x._1 == chars2.head).head
    			val cIndex = counts.indexOf(currentCount,0)
    			 addTimes(chars2.tail,(currentCount._1,currentCount._2+1) :: counts.drop(cIndex+1) ::: counts.take(cIndex))
    			
    		}
    		
    	}
    	
    	addTimes(chars,Nil)
    
    }                                             //> times: (chars: List[Char])List[(Char, Int)]
 
 
 val timeTest = times(List('a','b','c','a','b','c','a'))
                                                  //> timeTest  : List[(Char, Int)] = List((a,3), (c,2), (b,2))
  
  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
    def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs.sortWith( (x,y) =>x._2 < y._2).map(x=>new Leaf(x._1,x._2))
                                                  //> makeOrderedLeafList: (freqs: List[(Char, Int)])List[patmat.patmat.Huffman.L
                                                  //| eaf]
  
  val freqs = List(('a',2),('b',1),('c',4),('d',2))
                                                  //> freqs  : List[(Char, Int)] = List((a,2), (b,1), (c,4), (d,2))
  makeOrderedLeafList(freqs)                      //> res0: List[patmat.patmat.Huffman.Leaf] = List(Leaf(b,1), Leaf(a,2), Leaf(d,
                                                  //| 2), Leaf(c,4))
  
  
  
  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
    def singleton(trees: List[CodeTree]): Boolean = !trees.isEmpty && trees.tail.isEmpty
                                                  //> singleton: (trees: List[patmat.patmat.Huffman.CodeTree])Boolean
  
  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
    def combine(trees: List[CodeTree]): List[CodeTree] = {
    	if(trees.isEmpty || trees.tail.isEmpty) trees
    	else{
    		val newTree : CodeTree = new Fork(trees.head,trees.tail.head, chars(trees.head):::chars(trees.tail.head),weight(trees.tail.head) + weight(trees.head))
    		val lefttrees = trees.tail.tail
    		val newTreeWeight = weight(newTree)
    		 lefttrees.filter(x=>weight(x)<newTreeWeight)::: List(newTree) :::lefttrees.filter(x=>weight(x)>=newTreeWeight)
    	}
    	
    }                                             //> combine: (trees: List[patmat.patmat.Huffman.CodeTree])List[patmat.patmat.Hu
                                                  //| ffman.CodeTree]
  
  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
    def until[T](xxx: List[T]=>Boolean, yyy: (List[T] => List[T]))(zzz: List[T]): List[T] = {
    	if(xxx(zzz)) zzz else until(xxx,yyy)(yyy(zzz))
    }                                             //> until: [T](xxx: List[T] => Boolean, yyy: List[T] => List[T])(zzz: List[T])L
                                                  //| ist[T]
  
  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
    def createCodeTree(chars: List[Char]): CodeTree = {
    	until(singleton,combine)(makeOrderedLeafList(times(chars))).head
    }                                             //> createCodeTree: (chars: List[Char])patmat.patmat.Huffman.CodeTree
  
  
  val temp = makeOrderedLeafList(times(List('a','b','c','a','c','d','f','a','c','a')))
                                                  //> temp  : List[patmat.patmat.Huffman.Leaf] = List(Leaf(f,1), Leaf(d,1), Leaf(
                                                  //| b,1), Leaf(c,3), Leaf(a,4))
  combine(temp)                                   //> res1: List[patmat.patmat.Huffman.CodeTree] = List(Leaf(b,1), Fork(Leaf(f,1)
                                                  //| ,Leaf(d,1),List(f, d),2), Leaf(c,3), Leaf(a,4))
   
   
   combine(combine(temp))                         //> res2: List[patmat.patmat.Huffman.CodeTree] = List(Fork(Leaf(b,1),Fork(Leaf(
                                                  //| f,1),Leaf(d,1),List(f, d),2),List(b, f, d),3), Leaf(c,3), Leaf(a,4))
                                                  
  val myTree = createCodeTree(List('a','b','c','a','c','d','f','a','c','a'))
                                                  //> myTree  : patmat.patmat.Huffman.CodeTree = Fork(Leaf(a,4),Fork(Fork(Leaf(b,
                                                  //| 1),Fork(Leaf(f,1),Leaf(d,1),List(f, d),2),List(b, f, d),3),Leaf(c,3),List(b
                                                  //| , f, d, c),6),List(a, b, f, d, c),10)
  
  

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
    def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    
    	def decodeAndAdd(CurrentTree: CodeTree, bits: List[Bit],pre: List[Char]): List[Char] = CurrentTree match {
    			case Leaf(c,w)=> decodeAndAdd(tree,bits,pre::: List(c))

    			case Fork(l,r,_,_) =>{
    				if(bits.isEmpty) pre
    				else if(bits.head ==0) decodeAndAdd(l,bits.tail,pre)
    				else decodeAndAdd(r,bits.tail,pre)
    			}
    
    	}
    	
    	decodeAndAdd(tree,bits,Nil)
    }                                             //> decode: (tree: patmat.patmat.Huffman.CodeTree, bits: List[patmat.patmat.Huf
                                                  //| fman.Bit])List[Char]
                                                  
                                                  
                                                  
     decode(myTree,List(1,1,1,0,0,1,0,1,0,0))     //> res3: List[Char] = List(c, b, f, a)
  
  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)
                                                  //> frenchCode  : patmat.patmat.Huffman.CodeTree = Fork(Fork(Fork(Leaf(s,121895
                                                  //| ),Fork(Leaf(d,56269),Fork(Fork(Fork(Leaf(x,5928),Leaf(j,8351),List(x, j),14
                                                  //| 279),Leaf(f,16351),List(x, j, f),30630),Fork(Fork(Fork(Fork(Leaf(z,2093),Fo
                                                  //| rk(Leaf(k,745),Leaf(w,1747),List(k, w),2492),List(z, k, w),4585),Leaf(y,472
                                                  //| 5),List(z, k, w, y),9310),Leaf(h,11298),List(z, k, w, y, h),20608),Leaf(q,2
                                                  //| 0889),List(z, k, w, y, h, q),41497),List(x, j, f, z, k, w, y, h, q),72127),
                                                  //| List(d, x, j, f, z, k, w, y, h, q),128396),List(s, d, x, j, f, z, k, w, y, 
                                                  //| h, q),250291),Fork(Fork(Leaf(o,82762),Leaf(l,83668),List(o, l),166430),Fork
                                                  //| (Fork(Leaf(m,45521),Leaf(p,46335),List(m, p),91856),Leaf(u,96785),List(m, p
                                                  //| , u),188641),List(o, l, m, p, u),355071),List(s, d, x, j, f, z, k, w, y, h,
                                                  //|  q, o, l, m, p, u),605362),Fork(Fork(Fork(Leaf(r,100500),Fork(Leaf(c,50003)
                                                  //| ,Fork(Leaf(v,24975),Fork(Leaf(g,13288),Leaf(b,13822),List(g, b),27110),List
                                                  //| (v, g, b),52085),List(c
                                                  //| Output exceeds cutoff limit.

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)
                                                  //> secret  : List[patmat.patmat.Huffman.Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1,
                                                  //|  1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0,
                                                  //|  1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0,
                                                  //|  0, 1, 0, 1)

  /**
   * Write a function that returns the decoded secret
   */
    def decodedSecret: List[Char] = decode(frenchCode,secret)
                                                  //> decodedSecret: => List[Char]
  decodedSecret                                   //> res4: List[Char] = List(h, u, f, f, m, a, n, e, s, t, c, o, o, l)

  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
    def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    	def encodeAndAdd(currentTree : CodeTree, text: List[Char],bits:List[Bit]) : List[Bit] = {
    		if(text.isEmpty) bits
    		else currentTree match {
					case Leaf(_,_) => encodeAndAdd(tree,text.tail,bits)
					case Fork(l,r,_,_) => {
						if( chars(l).contains(text.head)) encodeAndAdd(l,text,bits::: List(0))
						else encodeAndAdd(r,text,bits::: List(1))
					}
    		
    		}
    	
    	}
    	
    	
    	
    	
    	encodeAndAdd(tree,text,Nil)
    }                                             //> encode: (tree: patmat.patmat.Huffman.CodeTree)(text: List[Char])List[patmat
                                                  //| .patmat.Huffman.Bit]
          
  encode(myTree)(List('c','b','f','a'))           //> res5: List[patmat.patmat.Huffman.Bit] = List(1, 1, 1, 0, 0, 1, 0, 1, 0, 0)
  
  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
    def codeBits(table: CodeTable)(char: Char): List[Bit] = table.filter(x=>x._1==char).head._2
                                                  //> codeBits: (table: patmat.patmat.Huffman.CodeTable)(char: Char)List[patmat.p
                                                  //| atmat.Huffman.Bit]
  
  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
    def convert(tree: CodeTree): CodeTable = tree match {
    		case Leaf(c,_)=> List((c,Nil))
    		
    		case Fork(l,r,_,_) => mergeCodeTables(convert(l),convert(r))
    		
    }                                             //> convert: (tree: patmat.patmat.Huffman.CodeTree)patmat.patmat.Huffman.CodeTa
                                                  //| ble
   
    
  
  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
    def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    	a.map(x=> (x._1,List(0):::x._2 )) ::: b.map(x=>(x._1,List(1):::x._2))
    }                                             //> mergeCodeTables: (a: patmat.patmat.Huffman.CodeTable, b: patmat.patmat.Huff
                                                  //| man.CodeTable)patmat.patmat.Huffman.CodeTable
                                                  
                                                  
   myTree                                         //> res6: patmat.patmat.Huffman.CodeTree = Fork(Leaf(a,4),Fork(Fork(Leaf(b,1),F
                                                  //| ork(Leaf(f,1),Leaf(d,1),List(f, d),2),List(b, f, d),3),Leaf(c,3),List(b, f,
                                                  //|  d, c),6),List(a, b, f, d, c),10)
    convert(myTree)                               //> res7: patmat.patmat.Huffman.CodeTable = List((a,List(0)), (b,List(1, 0, 0))
                                                  //| , (f,List(1, 0, 1, 0)), (d,List(1, 0, 1, 1)), (c,List(1, 1)))
  
  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
    def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    	
    	val table = convert(tree)
    	def encodeWithTable(t:CodeTable,text: List[Char]):List[Bit] = {
    		if(text.isEmpty) Nil else codeBits(table)(text.head)::: encodeWithTable(t,text.tail)
    	}
    	encodeWithTable(table,text)
    }                                             //> quickEncode: (tree: patmat.patmat.Huffman.CodeTree)(text: List[Char])List[
                                                  //| patmat.patmat.Huffman.Bit]
    quickEncode(myTree)(List('c','b','f','a'))    //> res8: List[patmat.patmat.Huffman.Bit] = List(1, 1, 1, 0, 0, 1, 0, 1, 0, 0)
                                                  //| 
  }
  
   