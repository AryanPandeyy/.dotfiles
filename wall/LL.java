package DSA;
public class LL {
  private class Node {
    private Node head;
    private Node tail;
    private int size;
    public void LL () {
      this.size = 0;
    }
    private int value;
    private Node next;
    public void insertFirst(int val) {
     Node node = new Node(val) ;
     node.next = head;
     head = node;
     if (tail == null) {
       tail = head;
     }
     size += 1;
    }
    public void display() {
      Node temp = head;
      while (temp != null) {
        System.out.print(temp.value + "->");
        temp = temp.next;
      }
      System.out.print("END");
    }

    public Node (int value) {
      this.value = value;
    }
    public Node(int value, Node next) {
      this.value = value;
      this.next = next;
    }
  }
}
