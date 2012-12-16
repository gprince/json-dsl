package jsonit.json;

import java.math.BigDecimal;

public interface ObjectBuilder<B extends Builder<?>> extends Builder<ObjectBuilder<?>> {

    ObjectBuilder<B> add(String name, BigDecimal value);

    ObjectBuilder<B> add(String name, Boolean value);
    
    ObjectBuilder<B> add(String name, Double value);

    ObjectBuilder<B> add(String name, Integer value);

    ObjectBuilder<B> add(String name, Long value);

    ObjectBuilder<B> add(String name, String value);

    B endObject();
}