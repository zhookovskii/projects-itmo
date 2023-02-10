package md2html;

import java.util.*;

class Text {
    private StringBuilder input;
    private static String[] mdTag;
    private static Map<String, String> special;
    private static Map<String, String> tag;
    {
        mdTag = new String[]{"*", "**", "_", "__", "--", "`", "%"};
        tag = Map.of (
                "*", "em",
                "**", "strong",
                "_", "em",
                "__", "strong",
                "--", "s",
                "`", "code",
                "%", "var"
        );
        special = Map.of(
                "<", "&lt;",
                ">", "&gt;",
                "&", "&amp;"
        );
    }
    Text(StringBuilder input) {
        this.input = input;
    }

    private String getTag(String s) {
        return tag.get(s);
    }

    private String isTag(String s) {
        String pos = tag.getOrDefault(s, null);
        if (pos == null) {
            pos = tag.getOrDefault(Character.toString(s.charAt(0)), null);
            s = Character.toString(s.charAt(0));
        }
        if (pos == null) {
            s = null;
        }
        return s;
    }

    public void toHtml(StringBuilder current) {
        Map<String, Integer> processed = new HashMap<>();
        for (String item : mdTag) {
            processed.put(item, 0);
        }
        Map<String, List<Integer>> tagPos = new HashMap<>();
        for (String value : mdTag) {
            tagPos.put(value, new ArrayList<>());
        }
        for (int i = 0; i < input.length(); i++) {
            if (input.charAt(i) == '\\') {
                i++;
                continue;
            }
            String s;
            if (i + 2 > input.length()) {
                s = input.substring(i, input.length());
            } else {
                s = input.substring(i, i + 2);
            }
            if (isTag(s) != null) {
                tagPos.get(isTag(s)).add(i);
            }
        }
        for (List<Integer> list : tagPos.values()) {
            if (list.size() % 2 == 1) {
                list.remove(list.get(list.size() - 1));
            }
        }
        for (int i = 0; i < input.length(); i++) {
            String s;
            if (i + 2 > input.length()) {
                s = input.substring(i, input.length());
            } else {
                s = input.substring(i, i + 2);
            }
            char ch = input.charAt(i);
            if (ch == '\\') {
                continue;
            }
            if (special.getOrDefault(Character.toString(ch), null) != null) {
                current.append(special.get(Character.toString(ch)));
                continue;
            }
            String curr = isTag(s);
            if (curr == null || tagPos.get(curr).size() == 0) {
                current.append(ch);
            } else {
                boolean isOpen = (tagPos.get(curr).size() - processed.get(curr)) % 2 == 0;
                processed.put(curr, processed.get(curr) + 1);
                String tag;
                if (isOpen) {
                    tag = "<" + getTag(isTag(s)) + ">";
                } else {
                    tag = "</" + getTag(isTag(s)) + ">";
                }
                current.append(tag);
                i += curr.length() - 1;
            }
        }
    }
}