function uuid_ish() {
  // got this from here: http://stackoverflow.com/a/2117523
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
    var r = Math.random()*16|0, v = c == 'x' ? r : (r&0x3|0x8);
    return v.toString(16);
  });
}

var delete_timeout = 3000;

var target_password;
var access_token;

// var api_base_url = "http://localhost:8000";
// var streaming_base_url = "ws://localhost:9000";

var api_base_url = "https://notify.uz/hnnotify";
var streaming_base_url = "wss://notify.uz/hnnotify";

var debugging_mode = false;
var last_event;

function parameterized_url(method) {
  return api_base_url + method +
    '?email=' + encodeURIComponent($('#email').val()) + '&' +
    'access-token=' + encodeURIComponent(access_token);
}

function parameterized_streaming_url(method) {
  return streaming_base_url + method +
    '?email=' + encodeURIComponent($('#email').val()) + '&' +
    'access-token=' + encodeURIComponent(access_token);
}

var listening_socket;

function new_notification_dom(type) {
  $('#notifications').append($('#notification_template').html());
  var dom = $('#notifications > :last')[0];
  $('h2', dom).text(type);
  return dom;
}

function parent_scroll_to_bottom(dom) {
  // this makes no sense to me, but i'm not complaining
  var parent = dom.parentElement.parentElement;
  parent.scrollTop = parent.scrollHeight;
}

function notification_slide_in(dom) {
  $(dom).show('slide', { to: { width: 200, height: 60} }, 1000, function() {});
}

function handle_item_change_event_field(event, field) {
  if (field == "kids") {
    var old_kids = event.fields[field][0];
    var new_kids = event.fields[field][1];
    var kid_addition = [];

    new_kids.forEach(function(new_kid) {
      // this new_kid is not in the old_kids
      if (old_kids.indexOf(new_kid) == -1) {
        kid_addition[kid_addition.length] = new_kid;
      }
    });

    var dom = new_notification_dom("Item Change");
    $('.notification_body', dom).append('<p>Post <a href="https://news.ycombinator.com/item?id=' + event.hn_id + '">#' + event.hn_id + '</a>\'s children have changed.</p>');
    if (kid_addition.length == 0) {
      $('.notification_body', dom).append('<p>Ranking order changed</p>');
    } else {
      $('.notification_body', dom).append('<p>New Kids:</p>');
      $('.notification_body', dom).append('<ul></ul>');
      var ul_dom = $('.notification_body ul', dom)[0];

      kid_addition.forEach(function(kid_id) {
        $(ul_dom).append('<li>Post <a href="https://news.ycombinator.com/item?id=' + kid_id + '">' + kid_id + '</a></li>');
      });
    }

    notification_slide_in(dom);
    parent_scroll_to_bottom(dom);
  }
  else if (field == "descendants") {
    // nothing, handled with "kids"
  }
  else if (field == "score") {
    var dom = new_notification_dom("Item Change");
    $('.notification_body', dom).append('<p>Post <a href="https://news.ycombinator.com/item?id=' + event.hn_id + '">#' + event.hn_id + '</a>\'s score has changed from ' + event.fields[field][0] + ' to ' + event.fields[field][1] + '</p>');
    notification_slide_in(dom);
    parent_scroll_to_bottom(dom);
  }
  else {
    var dom = new_notification_dom("Item Change");
    $('.notification_body', dom).append('<p>change event, unknown field: ' + field + '</p>');
    notification_slide_in(dom);
    parent_scroll_to_bottom(dom);
    if (debugging_mode) { stop_stream(); }
  }
}

function handle_profile_change_event_field(event, field) {
  if (field == "karma") {
    var dom = new_notification_dom("Profile Change");
    $('.notification_body', dom).append('<p><a href="https://news.ycombinator.com/user?id=' + event.hn_id + '">' + event.hn_id + '</a>\'s karma has changed from ' + event.fields[field][0] + ' to ' + event.fields[field][1] + '.</p>');
    notification_slide_in(dom);
    parent_scroll_to_bottom(dom);
  } else {
    var dom = new_notification_dom("Profile Change");
    $('.notification_body', dom).append('<p>change event, unknown field: ' + field + '</p>');
    notification_slide_in(dom);
    parent_scroll_to_bottom(dom);
    if (debugging_mode) { stop_stream(); }
  }
}

function handle_item_change_event(event) {
  Object.keys(event.fields).forEach(function(field) {
    handle_item_change_event_field(event, field);
  });
}

function handle_profile_change_event(event) {
  Object.keys(event.fields).forEach(function(field) {
    handle_profile_change_event_field(event, field);
  });
}

function handle_stream_event(event) {
  last_event = event;
  if (event.sub_type == "item_change") {
    handle_item_change_event(event);
  }
  else if (event.sub_type == "profile_change") {
    handle_profile_change_event(event);
  }
  else {
    var dom = new_notification_dom("Unknown");
    $('.notification_body', dom).append('<p>Unknown object type: ' + event.sub_type + '</p>');
    notification_slide_in(dom);
    parent_scroll_to_bottom(dom);
    if (debugging_mode) { stop_stream(); }
  }
}

function start_stream() {
  listening_socket = new WebSocket(
    parameterized_streaming_url('/notifications'));
  listening_socket.onmessage = function(e) {
    var parsed = JSON.parse(e.data);
    current_id = parsed._id;
    setTimeout(function() {
      if (listening_socket) {
        listening_socket.send(JSON.stringify({type: "remove_event", event_id: current_id}));
      }
    }, delete_timeout);

    handle_stream_event(parsed);
  };
  listening_socket.onclose = start_stream;
}

function stop_stream() {
  if (listening_socket) {
    listening_socket.onclose = function() {};
    if (!(listening_socket.readyState == WebSocket.CLOSED ||
          listening_socket.readyState == WebSocket.CLOSING)) {
      listening_socket.close();
    }
    listening_socket = false;
  }
}

function add_item(item) {
  $.get(
    parameterized_url('/current-api-user'),
    '',
    function(data) {
      var parsed = JSON.parse(data);
      parsed.items[parsed.items.length] = item;
      $.post(
        parameterized_url('/current-api-user'),
        JSON.stringify(parsed),
        function(data) {
          console.log("Added item");
        });
    });
}

function remove_item(item) {
  $.get(
    parameterized_url('/current-api-user'),
    '',
    function(data) {
      var parsed = JSON.parse(data);
      var new_items = [];
      for (var idx = 0; idx < parsed.items.length; idx++) {
        if (parsed.items[idx] != item) {
          new_items[new_items.length] = parsed.items[idx];
        }
      }
      parsed.items = new_items;
      $.post(
        parameterized_url('/current-api-user'),
        JSON.stringify(parsed),
        function(data) {
          console.log("Removed item");
        });
    });
}

function add_user(username) {
  $.get(
    parameterized_url('/current-api-user'),
    '',
    function(data) {
      var parsed = JSON.parse(data);
      parsed.users[parsed.users.length] = username;
      $.post(
        parameterized_url('/current-api-user'),
        JSON.stringify(parsed),
        function(data) {
          console.log("Added user");
        });
    });
}

function remove_user(username) {
  $.get(
    parameterized_url('/current-api-user'),
    '',
    function(data) {
      var parsed = JSON.parse(data);
      var new_users = [];
      for (var idx = 0; idx < parsed.users.length; idx++) {
        if (parsed.users[idx] != item) {
          new_users[new_users.length] = parsed.users[idx];
        }
      }
      parsed.users = new_users;
      $.post(
        parameterized_url('/current-api-user'),
        JSON.stringify(parsed),
        function(data) {
          console.log("Removed user");
        });
    });
}

function remove_dom_item(dom) {
  var num = parseInt($('.sample_item_num', dom).text());
  remove_item(num);
  $(dom).remove();
}

function remove_dom_user(dom) {
  var username = $('.sample_username', dom).text();
  remove_user(username);
  $(dom).remove();
}

function add_item_dom(item) {
  var current_item = item;
  $('#watched_items > ul').append($('#item_template').html());
  var new_dom = $('#watched_items > ul > :last')[0];
  $('#watched_items > ul > :last .sample_item_num').text('' + current_item);
  $('.ui-icon', new_dom).click(function() {
    remove_dom_item($(this).closest('li')[0]);
  });
}

function initialize_items(items) {
  $('#watched_items > ul').html('');
  for (var idx = 0; idx < items.length; idx++) {
    add_item_dom(items[idx]);
  }
}

function add_user_dom(username) {
  $('#watched_users > ul').append($('#user_template').html());
  var new_dom = $('#watched_items > ul > :last')[0];
  $('#watched_users > ul > :last .sample_username').text(username);
  $('.ui-icon', new_dom).click(function() {
    remove_dom_user($(this).closest('li')[0]);
  });
}

function initialize_users(users) {
  $('#watched_users > ul').html('');
  for (var idx = 0; idx < users.length; idx++) {
    add_user_dom(users[idx]);
  }
}

function load_watches() {
  $.get(
    parameterized_url('/current-api-user'),
    '',
    function(data) {
      var parsed = JSON.parse(data);
      initialize_items(parsed.items);
      initialize_users(parsed.users);
    });
}

function do_login() {
  $.post(
    api_base_url + '/login',
    JSON.stringify({ email: $('#email').val(),
                     password: target_password }),
    function(data) {
      var parsed = JSON.parse(data);
      console.log(parsed.message);
      access_token = parsed['access-token'];
      $('#login_info').hide();
      $('#logout_info').show();
      $('#logged_in_message').text(parsed.message);

      load_watches();
      start_stream();
    });
}

$(document).ready(function() {
  $('#issue_reset_code').click(function() {
    $.post(
      api_base_url + '/issue-reset-code',
      JSON.stringify({email: $('#email').val()}),
      function(data) {
        var parsed = JSON.parse(data);
        console.log(parsed.message);
        $('#issue_message').text(parsed.message);
      }
    );
  });
  $('#apply_reset_code').click(function() {
    if (!target_password) { target_password = uuid_ish(); }
    $.post(
      api_base_url + '/apply-reset-code',
      JSON.stringify({email: $('#email').val(),
                      "reset-code": $('#login_code').val(),
                      password: target_password}),
      function(data) {
        var parsed = JSON.parse(data);
        console.log(parsed.message);
        if (parsed.message != "Password reset") {
          $('#reset_login_message').text(parsed.message);
        } else {
          $('#issue_message').text('');
          $('#reset_login_message').text('');
          do_login();
        }
      }
    );
  });
  $('#logout').click(function () {
    $.post(
      parameterized_url('/logout'),
      '',
      function(data) {
        var parsed = JSON.parse(data);
        console.log(parsed.message);
        alert("Logged out");
        access_token = false;
        $('#logout_info').hide();
        $('#login_info').show();
        stop_stream();
      }
    );
  });
  $('#add_item').click(function() {
    var item = parseInt($('#new_item').val());
    add_item_dom(item);
    add_item(item);
    $('#new_item').val('');
  });
  $('#add_user').click(function() {
    var user = $('#new_user').val();
    add_user_dom(user);
    add_user(user);
    $('#new_user').val('');
  });
  $('#stop_start_stream').click(function() {
    if (listening_socket) {
      stop_stream();
      alert("stopped stream");
    } else {
      start_stream();
      alert("start stream");
    }
  });
});
