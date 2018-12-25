package org.evensen.util;

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

/**
 * Parses option lines according to a specified syntax.
 * 
 * @author Pelle Evensen
 */
public class GetOptions {
	public enum Arity {
		/**
		 * A flag, taking <strong>no</strong> arguments.
		 */
		NONE(" -"),

		/**
		 * Option taking exactly <strong>one</strong> argument.
		 */
		ONE(" ?"),

		/**
		 * Option taking <strong>one or more</strong> arguments.
		 */
		MANY(" +");


		private final String s;

		private Arity(String s) {
			this.s = s;
		}

		@Override
		public String toString() {
			return s;
		}
	}

	private static String legalChars = "[a-zA-Z0-9+-]";
	private Pattern optPatt = Pattern.compile("(-" + legalChars + "+)$");
	private Map<String, Arity> o;
	private Map<String, String> synonyms;
	private Map<String, List<String>> matched;
	private List<String> looseArguments;
	private boolean acceptLooseArguments;
	private boolean successfulParse;

	/**
	 * Creates a new <code>GetOptions</code> object.
	 */
	public GetOptions() {
		o = new HashMap<String, Arity>();
		synonyms = new HashMap<String, String>();
		matched = null;
		acceptLooseArguments = false;
		looseArguments = null;
		successfulParse = false;
	}

	/**
	 * Creates a new <code>GetOptions</code> object with the options
	 * from <code>opts</code>.
	 * @see #add(List, List)
	 */
	public GetOptions(List<String> optionNames, List<Arity> arities) {
		this();
		add(optionNames, arities);
	}

	/**
	 * Sets whether this object should accept "loose arguments" or
	 * not.  A loose argument is defined as a string that isn't tied
	 * to a specific flag.  
	 * <p>
	 * If one were to build for example
	 * <code>ls</code>, one would want to accept loose arguments since
	 * <code>"foo"</code> and <code>"bar"</code> in <code>"ls -l foo
	 * bar"</code> are not tied to the <code>"-l"</code> option.
	 * <p>The default behaviour is to reject loose arguments.
	 */
	public void setAcceptLooseArguments(boolean accept) {
		acceptLooseArguments = accept;
	}

	/**
	 * Sets the object to reject loose arguments.
	 */
	public void rejectLooseArguments() {
		setAcceptLooseArguments(false);
	}

	/**
	 * Sets the object to accept loose arguments.
	 */
	public void acceptLooseArguments() {
		setAcceptLooseArguments(true);
	}

	/**
	 * Returns whether this object accepts loose arguments or not.
	 */
	public boolean getAcceptLooseArguments() {
		return acceptLooseArguments;
	}

	/**
	 * Returns whether the last parsing was successful or not.
	 */
	public boolean parsed() {
		return successfulParse;
	}

	/**
	 * Adds an option to this object.  Accepted options are on the
	 * form <code>'-[a-zA-Z0-9-+]+'</code>, that
	 * is: A minus followed by one or more alphanumerical characters or +/-.
	 *<p> 
	 * The <code>arity</code> argument sets how many arguments the option takes;
	 * {@link Arity#NONE} indicates no arguments,
	 * {@link Arity#ONE} exactly one argument and
	 * {@link Arity#MANY} one or more arguments.  
	 */
	public void add(String opt, Arity arity) {
		Matcher match;

		match = optPatt.matcher(opt);

		if(match.find()) {
			String option;

			option = match.group(1);

			if(option != null && arity != null) {
				if(o.get(option) != null) {
					throw new IllegalArgumentException("Option " + option +
					" inserted twice."); 
				}
				o.put(option, arity);
				if(false) {
					System.out.println("Added opt " + option + " as " +
							o.get(option).name());
				}
			}
		} else {
			throw new IllegalArgumentException("Bad format for option/parameter; " +
					opt);
		}
	}

	public void addSynonym(String opt, String synonymousTo) {
		if(o.get(opt) != null) {
			throw new IllegalArgumentException(opt + " is already an option");
		} else if(synonyms.get(opt) != null) {
			throw new IllegalArgumentException(opt + " is already a synonym");
		} else if(o.get(synonymousTo) == null) { 
			throw new IllegalArgumentException(synonymousTo + " is not a defined option");
		} else if(synonyms.get(synonymousTo) != null) { 
			throw new IllegalArgumentException(synonymousTo + " is already a synonym");
		}

		synonyms.put(opt, synonymousTo);
	}

	/**
	 * Adds all options in <code>opts</code>.
	 */
	public void add(List<String> optionNames, List<Arity> arities) {
		if(optionNames.size() != arities.size()) {
			if(optionNames.size() < arities.size()) {
				throw new IllegalArgumentException("The number of arities is greater than " +
				"the number of options");
			}
			throw new IllegalArgumentException("The number of options is greater than " +
			"the number of arities");
		}
		Iterator<String> oIt = optionNames.iterator();
		Iterator<Arity> aIt = arities.iterator();

		while(oIt.hasNext()) {
			add(oIt.next(), aIt.next());
		}
	}

	/**
	 * Returns the arguments for <code>option</code>, provided <code>option</code>
	 * is set to take one or many arguments.
	 * @return The list of arguments in case the option was matched. 
	 * @throws IllegalArgumentException If <code>option</code> does not take any arguments. 
	 */
	public List<String> matched(String option) {
		if(o.get(option) == Arity.NONE &&
				matched.get(option).size() > 0 &&
				matched.get(option).get(0).length() > 0) {
			throw new IllegalArgumentException("Option \"" + option + " is set to accept " +
			"no arguments.");
		}

		return matched.get(option);
	}

	/**
	 * Check if a flag was set or not.
	 * @return <code>true</code> if the flag was set, otherwise <code>false</code>.
	 * @throws IllegalArgumentException If <code>option</code> expects any arguments.
	 */
	public boolean isSet(String option) {
		if(o.get(option) != Arity.NONE) {
			throw new IllegalArgumentException("Option \"" + option + " is set to accept " +
			"arguments.");
		}

		return matched.get(option) != null;
	}

	/**
	 * Gives loose arguments from the last parsing.
	 * @return Empty list if there are no loose arguments or this object is
	 * set to reject them.
	 */
	public List<String> looseArguments() {
		return acceptLooseArguments ? looseArguments : new LinkedList<String>();
	}

	public Map<String,List<String>> getMatches() {
		Map<String,List<String>> m = new LinkedHashMap<String,List<String>>();
		m.putAll(matched);

		return m;
	}

	/**
	 * Extracts valid options.
	 */
	public List<String> getValidOptions() {
		return new ArrayList<String>(o.keySet());
	}

	/**
	 * Checks that <code>args</code> is correct according to this
	 * object's syntax.
	 * @return false if <code>args</code> is inconsistent with this
	 * object's syntax, otherwise true.
	 */
	public boolean parse(String[] args) {
		List<String> looseArgs = new ArrayList<String>();
		Map<String, List<String>> tmpOpts = new LinkedHashMap<String, List<String>>();
		String currentOpt = null;
		Arity p = Arity.NONE;
		Arity curArgs = null;

		for(String arg : args) {
			// Check if the argument is among the known options.
			if((p = o.get(arg)) != null) {
				curArgs = p;
				currentOpt = arg;

				if(curArgs == Arity.NONE) {
					tmpOpts.put(arg, new ArrayList<String>(1));
					tmpOpts.get(arg).add(""); // Insert something that is non-null.
					currentOpt = null;
				} else if(curArgs == Arity.ONE || curArgs == Arity.MANY) {
					if(tmpOpts.get(arg) == null) {
						tmpOpts.put(arg, new ArrayList<String>());
					}
				}
			} else {
				if(curArgs == Arity.MANY) {
					tmpOpts.get(currentOpt).add(arg);
				} else if(curArgs == Arity.ONE) {
					tmpOpts.get(currentOpt).add(arg);
					// Reject further arguments.
					curArgs = Arity.NONE;
				} else {
					if(acceptLooseArguments == true) {
						looseArgs.add(arg);
					} else {
						// Malformed argument list, we don't expect
						// any more arguments for this option.
						return successfulParse = false;
					}
				}
			}
		}

		matched = new LinkedHashMap<String, List<String>>();

		// Copy to lists and check for sanity.
		for(Map.Entry<String,List<String>> e : tmpOpts.entrySet()) {

			// Each argument vector should contain at least 1
			// element. Argument-less options still have some
			// (arbitrary) string attached to generate non-null
			// arrays.
			if(e.getValue().size() == 0) {
				successfulParse = false;
				// Clear possible "remainder state" before we return.
				matched = new LinkedHashMap<String, List<String>>();
				looseArguments = null;

				return false;
			}

			matched.put(e.getKey(), e.getValue());
		}

		if(acceptLooseArguments == true) {
			looseArguments = new ArrayList<String>(looseArgs);
		} else {
			looseArguments = new ArrayList<String>();
		}

		return successfulParse = true;
	}
}
