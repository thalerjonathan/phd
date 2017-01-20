package agent;

import java.util.HashMap;

/**
 * Created by jonathan on 20/01/17.
 */
public class Message<T extends Comparable<T>> implements Comparable<Message<T>> {
    private T type;
    private HashMap<String, Object> values;

    public Message(T t) {
        this.type = t;
    }

    public T getType() {
        return this.type;
    }

    public Object getValue(String k) {
        return this.values.get(k);
    }

    public void addValue(String k, Object v) {
        if ( null == this.values )
            this.values = new HashMap<>();

        this.values.put(k, v);
    }

    @Override
    public int compareTo(Message<T> o) {
        return o.type.compareTo(this.type);
    }

    public boolean isOfType(T t) {
        return this.type.compareTo(t) == 0;
    }
}
