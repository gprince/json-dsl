package json.builder;

import json.lang.JsonValue.JsonStructure;

/**
 * Contrat pour le builder racine
 * 
 * @author gprince
 * 
 * @param <V>
 */
public interface JsonBuildable<V extends JsonStructure<?>> extends
		JsonBuilder {

	/**
	 * Construit l'object final
	 * 
	 * @return une instance de JsonStructure
	 */
	V build();
}