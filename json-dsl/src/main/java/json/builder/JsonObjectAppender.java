package json.builder;

import java.math.BigDecimal;

import json.lang.JsonValue.JsonArray;
import json.lang.JsonValue.JsonObject;
import json.lang.exception.JsonBuildException;

/**
 * @author gprince
 * 
 * @param <T>
 */
public interface JsonObjectAppender<T extends JsonBuilder> {

	/**
	 * Commence un nouveau JsonArray � rajouter au JsonObject en cours de
	 * construction
	 * 
	 * @param name
	 *            le nom du nouveau JsonArray dans le JsonObject en cours de
	 *            construction
	 * 
	 * @return un nouveau JsonStructure pour le nouveau JsonArray � construire
	 *         et � rajouter
	 */
	JsonArrayBuilder<T> startArray(String name) throws JsonBuildException;

	/**
	 * Commence un nouveau JsonObject � rajouter au JsonObject en cours de
	 * construction
	 * 
	 * @param name
	 *            le nom du nouveau JsonObject dans le JsonObject en cours de
	 *            construction
	 * 
	 * @return un nouveau JsonStructure pour le nouveau JsonObject � construire
	 *         et � rajouter
	 * @throws JsonBuildException
	 */
	JsonObjectBuilder<T> startObject(String name) throws JsonBuildException;

	/**
	 * Ajoute un {@link BigDecimal} comme membre du JsonObject � construire
	 * 
	 * @param name
	 *            nom du membre
	 * @param value
	 *            valeur du membre
	 * @return le courant afin de pouvoir cha�ner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(String name, BigDecimal value) throws JsonBuildException;

	/**
	 * Ajoute un {@link Boolean} comme membre du JsonObject � construire
	 * 
	 * @param name
	 *            nom du membre
	 * @param value
	 *            valeur du membre
	 * @return le courant afin de pouvoir cha�ner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(String name, Boolean value) throws JsonBuildException;

	/**
	 * Ajoute un {@link Double} comme membre du JsonObject � construire
	 * 
	 * @param name
	 *            nom du membre
	 * @param value
	 *            valeur du membre
	 * @return le courant afin de pouvoir cha�ner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(String name, Double value) throws JsonBuildException;

	/**
	 * Ajoute un {@link Integer} comme membre du JsonObject � construire
	 * 
	 * @param name
	 *            nom du membre
	 * @param value
	 *            valeur du membre
	 * @return le courant afin de pouvoir cha�ner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(String name, Integer value) throws JsonBuildException;

	/**
	 * Ajoute un {@link Long} comme membre du JsonObject � construire
	 * 
	 * @param name
	 *            nom du membre
	 * @param value
	 *            valeur du membre
	 * @return le courant afin de pouvoir cha�ner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(String name, Long value) throws JsonBuildException;

	/**
	 * Ajoute un {@link String} comme membre du JsonObject � construire
	 * 
	 * @param name
	 *            nom du membre
	 * @param value
	 *            valeur du membre
	 * @return le courant afin de pouvoir cha�ner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(String name, String value) throws JsonBuildException;

	/**
	 * Ajoute un {@link JsonObject} comme membre du JsonObject � construire
	 * 
	 * @param name
	 *            nom du membre
	 * @param value
	 *            valeur du membre
	 * @return le courant afin de pouvoir cha�ner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(String name, JsonObject object) throws JsonBuildException;

	/**
	 * Ajoute un {@link JsonArray} comme membre du JsonObject � construire
	 * 
	 * @param name
	 *            nom du membre
	 * @param value
	 *            valeur du membre
	 * @return le courant afin de pouvoir cha�ner les appels
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T add(String name, JsonArray array) throws JsonBuildException;

	/**
	 * Merge le JsonObject pass� en param�tre avec le JsonObject en cours de
	 * construction
	 * 
	 * <p>
	 * Tous les memebres du JsonObject � merger sont rajout�s comme membres du
	 * JsonObject en cours de construction
	 * </p>
	 * 
	 * @param object
	 *            le JsonObject � merger
	 * @return
	 * @throws JsonBuildException
	 *             si le nom est null
	 */
	T merge(JsonObject object) throws JsonBuildException;
}