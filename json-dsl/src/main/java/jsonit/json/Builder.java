package jsonit.json;

public interface Builder<B extends Builder<?>> {

    ObjectBuilder<B> startObject(String name);

    ArrayBuilder<B> startArray();
}