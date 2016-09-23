{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Frontend.BackboneTemplates where

import Data.Text
import Text.RawString.QQ

backboneTemplates :: Text
backboneTemplates = [r|
<script type="text/html" id="template-timer-single">
  <div class="fu">
    <span class="time <%= single.get('penalty') %>"><%= formatTime(single.get("time")) %></span>
    <div class="edit">
      <a href="#" class="dnf">DNF</a>
      <a href="#" class="plus2">+2</a>
      <a href="#" class="delete">X</a>
    </div>
  </div>
</script>

<script type="text/html" id="template-timer-singles">
  <p class="suggestion">
    You haven't solved any cubes in the last 24 hours. :(
  </p>
  <ol>
  </ol>
  <section class="load-more">
    <a href="#">Show more solves</a>
  </section>
</script>

<script type="text/html" id="template-timer-timer">
  <small class="scramble">
    <%= formatScramble(scramble) %>
  </small>
  <div class="time-container">
    <div class="time">
      <%= formatTime(timer.currentTime()) %>
    </div>
    <p class="help">
      <% if (timer.isCountdownRunning() || !timer.hasInspection()) { %>
        Press Space to start/stop the timer.
      <% } else { %>
        Press Space to start the countdown.
      <% } %>
    </p>
    <label class="inspection-toggle"><input type="checkbox" class="inspection-toggle"<%= timer.hasInspection() ? " checked" : "" %>>15s Inspection</label>
  </div>
  <form accept-charset="UTF-8" class="formtastic single" id="new_single">
    <fieldset class="inputs">
      <ol>
        <li class="string input stringish" id="single_human_time_input"><label class="label" for="single_human_time">Time</label><input autocomplete="off" id="single_human_time" name="single[human_time]" type="text" value="" />
        </li>
      </ol>
    </fieldset>
    <fieldset class="buttons">
      <ol>
        <li class="commit button">
          <input class="create" name="commit" type="submit" value="Submit" />
        </li>
      </ol>
    </fieldset>
  </form>
  <div class="add_comment">
    <div class="bubble">
      <a href="#" class="add_comment">Comment your solve</a>
      <form accept-charset="UTF-8" id="add_comment" class="comment">
        <ol>
          <li>
            <textarea name="comment" cols="40" rows="4"></textarea>
          </li>
          <li>
            <input type="submit" value="Submit" />
          </li>
        </ol>
      </form>
    </div>
  </div>

  <a href="#" class="toggle">Set times manually</a>
</script>

<script type="text/html" id="template-timer-chart">
  <p class="tabs">
    Chart:
    <a href="#" class="by-solve selected">Recent</a>
    <a href="#" class="by-date">Longtime</a>
  </p>
  <div id="chart"></div>
  <p class="help">Zoom in by clicking and dragging with your mouse.</p>
  <input id="user-tokens" name="user-tokens" type="text" />
</script>

<script type="text/html" id="template-timer-stats">
  <div class="current">
    <h4>Current</h4>
    <% for (var count in currentAverages) { %>
      <% if (currentAverages.hasOwnProperty(count)) { %>
        <div class="avg<%= count %>">
          <% if (count === "100") { %>
            Mean of <%= count %>:
          <% } else { %>
            Average of <%= count %>:
          <% } %>
          <strong><%= formatTime(currentAverages[count]) %></strong>
          <small><a href="#" class="details" data-count="<%= count %>">Details</a></small><br />
        </div>
      <% } %>
    <% } %>
  </div>
  <!-- TODO: fix semantic of stats (table / dl) -->
  <div class="best">
    <h4>Best</h4>
    <% for (var i = 0; i < records.length; i++) { %>
      <% var r = records[i]; %>
      <% if (_.isObject(r)) { %>
        <%= r.capitalizedTitle() %>:
        <strong><%= formatTime(r.get("time")) %></strong>
        <small><a href="<%= r.getHtmlUrl(userSlug) %>">Details</a></small><br />
      <% } %>
    <% } %>
  </div>
</script>

<script type="text/html" id="template-timer-average-detail">
  <table class="singles">
    <thead>
      <tr>
        <th>Solve</th>
        <th>Time</th>
        <th>Scramble</th>
        <th>Comment</th>
      </tr>
    </thead>
    <tbody>
      <% for (var i = 0; i < singles.length; i++) { %>
        <% var single = singles[i]; %>
        <tr class="<%= i % 2 === 0 ? "even" : "odd" %>">
          <td><%= i + 1 %>.</td>
          <td><strong class="time<%= single.dnf() ? " dnf" : "" %>"><%=formatTime(single.get("time")) %></strong></td>
          <td><small><%= single.get("scramble") %></small></td>
          <td><%= single.get("comment") %></td>
        </tr>
      <% } %>
    </tbody>
  </table>
</script>

<script type="text/html" id="template-timer-index">
  <div id="timer-left">
    <section id="timer">
    </section>
    <section id="stats">
    </section>
  </div>

  <div id="times">
    <div id="chart-container">
    </div>
    <aside id="singles">
    </aside>
  </div>
</script>
|]
