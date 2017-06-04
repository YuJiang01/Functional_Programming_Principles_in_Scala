package forcomp

object AnagramsTest {
  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)];import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(891); 

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
   val w = "abcfe";System.out.println("""w  : String = """ + $show(w ));$skip(138); 
   
 //val dictionary: List[Word] = List("Aarhus","Aaron","aback","abaft","ate","eat","tea")
 val dictionary: List[Word] = loadDictionary;System.out.println("""dictionary  : List[forcomp.AnagramsTest.Word] = """ + $show(dictionary ));$skip(26); 

println(dictionary.head);$skip(328); 
  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
   val x="abca";System.out.println("""x  : String = """ + $show(x ));$skip(20); 
   val y = x.toList;System.out.println("""y  : List[Char] = """ + $show(y ));$skip(153); 
   
  def wordOccurrences(w: Word): Occurrences =
  (w.toLowerCase.toList.groupBy(x=>x) map{case (k,v) => (k,v.size)} toList).sortWith((x,y)=>x._1<y._1);System.out.println("""wordOccurrences: (w: forcomp.AnagramsTest.Word)forcomp.AnagramsTest.Occurrences""");$skip(109); 

  /** Converts a sentence into its character occurrence list. */
  
  val z = List("dbc","Bcd","cas","add");System.out.println("""z  : List[String] = """ + $show(z ));$skip(25); 
  val m = z.mkString("");System.out.println("""m  : String = """ + $show(m ));$skip(89); 
  
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString(""));System.out.println("""sentenceOccurrences: (s: forcomp.AnagramsTest.Sentence)forcomp.AnagramsTest.Occurrences""");$skip(24); val res$0 = 

sentenceOccurrences(z);System.out.println("""res0: forcomp.AnagramsTest.Occurrences = """ + $show(res$0));$skip(720); 
  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
  dictionary.groupBy(x=>wordOccurrences(x));System.out.println("""dictionaryByOccurrences: => Map[forcomp.AnagramsTest.Occurrences,List[forcomp.AnagramsTest.Word]]""");$skip(30); val res$1 = 

dictionaryByOccurrences.head;System.out.println("""res1: (forcomp.AnagramsTest.Occurrences, List[forcomp.AnagramsTest.Word]) = """ + $show(res$1));$skip(135); val res$2 = 
                                                  
dictionaryByOccurrences.contains(List(('e',1), ('i',1), ('l',1), ('r',1), ('t',2)));System.out.println("""res2: Boolean = """ + $show(res$2));$skip(144); 

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word));System.out.println("""wordAnagrams: (word: forcomp.AnagramsTest.Word)List[forcomp.AnagramsTest.Word]""");$skip(31); val res$3 = 
   
   
   wordAnagrams("ate");System.out.println("""res3: List[forcomp.AnagramsTest.Word] = """ + $show(res$3));$skip(1203); 
  

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
  	if(occurrences.isEmpty)  List(occurrences)
  	else {
  		val nextCombinations = combinations(occurrences.tail)
  		val currentChar = occurrences.head._1
  		val currentCount = occurrences.head._2
  	(nextCombinations::	(for(i<- 1 to currentCount) yield addCharToOccurrencesList((currentChar,i),nextCombinations)).toList).flatten
 		 }
  };System.out.println("""combinations: (occurrences: forcomp.AnagramsTest.Occurrences)List[forcomp.AnagramsTest.Occurrences]""");$skip(140); 
  
  def addCharToOccurrencesList(chr:(Char,Int),OccList: List[Occurrences]) : List[Occurrences] = {
		for(occ<-OccList) yield chr::occ
  };System.out.println("""addCharToOccurrencesList: (chr: (Char, Int), OccList: List[forcomp.AnagramsTest.Occurrences])List[forcomp.AnagramsTest.Occurrences]""");$skip(44); val res$4 = 
  
  combinations(List(('a', 2), ('b', 2)));System.out.println("""res4: List[forcomp.AnagramsTest.Occurrences] = """ + $show(res$4));$skip(462); 

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
   
  val x1 = List(('a',2),('b',5));System.out.println("""x1  : List[(Char, Int)] = """ + $show(x1 ));$skip(247); 
   
   
   
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  		wordOccurrences(removeChar(x.foldLeft(List[Char]())((b,a)=>b:::List.fill(a._2)(a._1)),
 		 	y.foldLeft(List[Char]())((b,a)=>b:::List.fill(a._2)(a._1))).mkString)
  	};System.out.println("""subtract: (x: forcomp.AnagramsTest.Occurrences, y: forcomp.AnagramsTest.Occurrences)forcomp.AnagramsTest.Occurrences""");$skip(233); 
def removeChar(charList1 : List[Char], charList2: List[Char]): List[Char]={
	if(charList2.isEmpty) charList1
	else {
	val i = charList1.indexOf(charList2.head)
	removeChar(charList1.take(i):::charList1.drop(i+1),charList2.tail)
	}
};System.out.println("""removeChar: (charList1: List[Char], charList2: List[Char])List[Char]""");$skip(51); val res$5 = 

	subtract(List(('a', 2), ('b', 2)),List(('b',1)));System.out.println("""res5: forcomp.AnagramsTest.Occurrences = """ + $show(res$5));$skip(2430); 

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  /* def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
  	val senOccurs = sentenceOccurrences(sentence)
  	GetSetenceAnagram(senOccurs).flatMap(x=>convertOccurrenceToString(x))
  
  }*/
  def convertOccurrenceToString(occurList:List[Occurrences] ) : List[Sentence] = {
  	lazy val wordList = occurList.map(x=>dictionaryByOccurrences(x))
  	def getCombination(wl:List[List[Word]], prefix: List[Sentence]): List[Sentence] = {
  		if(wl.isEmpty) prefix
  		else {
  			val res = for(word<-wl.head) yield prefix.map(x=>word::x)
  			getCombination(wl.tail,res.foldLeft(List[Sentence]())((b,a)=>b:::a))
  		}
  	}
  	
  	getCombination(wordList,List[Sentence](List[Word]()))
  };System.out.println("""convertOccurrenceToString: (occurList: List[forcomp.AnagramsTest.Occurrences])List[forcomp.AnagramsTest.Sentence]""");$skip(102); val res$6 = 
  
   
  convertOccurrenceToString(List(List(('a',1),('e',1),('t',1)),List(('d',1),('g',1),('o',1))));System.out.println("""res6: List[forcomp.AnagramsTest.Sentence] = """ + $show(res$6));$skip(587); 
  
  
  
  
  def GetSetenceAnagram(occur: Occurrences) : List[List[Occurrences]] = {
  
  	if(occur.isEmpty) List[List[Occurrences]](List[Occurrences]())
  	else{
  		lazy val allSubsets : List[Occurrences] = combinations(occur)
  		if(allSubsets.forall(x => !dictionaryByOccurrences.contains(x))) List(List(List(('a',10))))
  		else {
  			val res =	for(s <- allSubsets if dictionaryByOccurrences.contains(s)) yield GetSetenceAnagram(subtract(occur,s)).map(x=>s::x)
  			res.foldLeft(List[List[Occurrences]]())((b,a)=>b:::a).filter(x=> !(x.contains(List(('a',10)))))
  		}
  	}
  
  };System.out.println("""GetSetenceAnagram: (occur: forcomp.AnagramsTest.Occurrences)List[List[forcomp.AnagramsTest.Occurrences]]""");$skip(38); 
  val occur = wordOccurrences("asis");System.out.println("""occur  : forcomp.AnagramsTest.Occurrences = """ + $show(occur ));$skip(395); 
  
  		
  		//yield GetSetenceAnagram(subtract(occur,s)).map(x=>s::x)
  		//res.foldLeft(List[List[Occurrences]]())((b,a)=>b:::a)
  
  
 // GetSetenceAnagram(wordOccurrences("abelson"))
  
 // List(List(('a',1), ('s',2)), List(('a',10))).contains(List(('a',10)))
  
  /*def GetAnagrams(occur:Occurrences, suffix: List[Sentence]) :List[Sentence] = {
  
  }*/
  
  val  sentence = List("as","is");System.out.println("""sentence  : List[String] = """ + $show(sentence ));$skip(48); 
  val senOccurs = sentenceOccurrences(sentence);System.out.println("""senOccurs  : forcomp.AnagramsTest.Occurrences = """ + $show(senOccurs ));$skip(73); val res$7 = 
  	GetSetenceAnagram(senOccurs).flatMap(x=>convertOccurrenceToString(x));System.out.println("""res7: List[forcomp.AnagramsTest.Sentence] = """ + $show(res$7));$skip(293); 
                                                  
                                                  
	def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
  	val senOccurs = sentenceOccurrences(sentence)
  	GetSetenceAnagram(senOccurs).flatMap(x=>convertOccurrenceToString(x))
  
  };System.out.println("""sentenceAnagrams: (sentence: forcomp.AnagramsTest.Sentence)List[forcomp.AnagramsTest.Sentence]""");$skip(36); val res$8 = 
  sentenceAnagrams(List("as","is"));System.out.println("""res8: List[forcomp.AnagramsTest.Sentence] = """ + $show(res$8));$skip(42); val res$9 = 
  
  sentenceAnagrams(List("Yes", "man"));System.out.println("""res9: List[forcomp.AnagramsTest.Sentence] = """ + $show(res$9))}
 
 /*val ss = List(1,2,3,4)
 val a = List(List(5,6),List(7,8))
 for(s<-ss) yield s::a*/
  
  
  }
