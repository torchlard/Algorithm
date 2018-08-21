import static java.lang.System.out;
import java.lang.String;

public class LinkedList {
  public static void main(String[] args) {
    
    int[] data = new int[] {3,6,9,2,5,8,1};
    Linked_list result = new Linked_list();
    
    result.insertAll(data);
    result.printAll();
    result.insert(4);
    result.insert(7);
    result.delete(9);
    result.delete(11);
    result.insert(90);
    result.printAll();
    result.search(14);
    
  }
}

class Element {
  public int value;
  public Element ref; 
  Element(int val){
    value = val;
  }
}

class Linked_list {
  private Element head;
  private Element tail;
  public void insert(int x){
    if(head == null){
      head = new Element(x);
      tail = head;
    } else {
      Element elem = new Element(x);
      tail.ref = elem;
      tail = elem;
    }
  }
  public void insertAll(int[] x_list){
    for(int i:x_list)
      insert(i);
  }
  public void delete(int target){
    Element current = head;
    if (current.ref != null){
      Element front = search_helper(target);
      if (front == null){
        out.println(target+" : no such element");
        return;
      }
      Element middle = front.ref;
      if (middle.ref != null) 
        front.ref = middle.ref;
      middle.ref = null;
    } else if (current.value==target && current!=null){
      head = null;
      tail = null;
    }
  }
  public void search(int target){
    if (search_helper(target) != null){
      out.println("value exist");
    } else {
      out.println("not exist");
    }
  }
  public Element search_helper(int target){
    Element current = head;
    while(true){
      if (current.ref == null) return null;
      if ((current.ref).value == target) return current;
      current = current.ref;
    }
  }
  public void printAll(){
    Element x = head;
    String print_list = "";
    while(true){
      print_list += x.value+" ";
      if (x.ref == null)
        break;
      x = x.ref;
    }
    out.println(print_list);
  }
}








