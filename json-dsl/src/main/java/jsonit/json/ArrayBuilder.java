package jsonit.json;

import java.math.BigDecimal;

public interface ArrayBuilder<B extends Builder<?>> extends Builder<ArrayBuilder<?>> {
    
    ArrayBuilder<B> add(BigDecimal value);

    ArrayBuilder<B> add(Boolean value);
    
    ArrayBuilder<B> add(Double value);

    ArrayBuilder<B> add(Integer value);

    ArrayBuilder<B> add(Long value);

    ArrayBuilder<B> add(String value);

    B endArray();
}