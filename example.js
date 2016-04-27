function uuid_ish() {
  // got this from here: http://stackoverflow.com/a/2117523
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
    var r = Math.random()*16|0, v = c == 'x' ? r : (r&0x3|0x8);
    return v.toString(16);
  });
}

var delete_timeout = 1000;

var target_password;
var access_token;

var api_base_url = "https://bcaine.com/hnnotify";
var streaming_base_url = "wss://bcaine.com/hnnotify";

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

function start_stream() {
  listening_socket = new WebSocket(
    parameterized_streaming_url('/notifications'));
  listening_socket.onmessage = function(e) {
    var parsed = JSON.parse(e.data);
    current_id = parsed._id;
    $('#events').val(JSON.stringify(parsed, null, 2));
    setTimeout(function() {
      listening_socket.send(JSON.stringify({type: "remove_event", event_id: current_id}));
    }, delete_timeout);

    $('#notifications').append($('#notification_template').html());
    $('#notifications > :last > h2').text("Notification");
    $('#notifications > :last .notification_body').append('<pre></pre>');
    $('#notifications > :last .notification_body > :last').text(JSON.stringify(parsed));
    $('#notifications > :last').show('slide', { to: { width: 200, height: 60} }, 1000, function() {});
    var parent = $('#notifications')[0];
    parent.scrollTop = parent.scrollHeight;
  };
  listening_socket.onclose = start_stream;
}

function stop_stream() {
  if (listening_socket) {
    listening_socket.onclose = function() {};
    listening_socket.close();
    listening_Socket = false;
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
          console.log("Added user");
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
});
