// // TODO: How can we use CommonJS modules?
// import * as React from 'react';

// WORKAROUND: Use global `React` on `window`:
const React = window.React;

interface Props {
  items: Array<Conversation>
}

export interface Conversation {
  id: string;
  type: string;
  unreadCount: number;
  verified: number;
  tokens?: (string)[] | null;
  active_at: number;
  lastMessage: string;
  timestamp: number;
  profileKey: object;
  profileName: string;
  profileAvatar: ProfileAvatar;
  name: string;
  avatar: Avatar;
  color?: null;
  expireTimer?: null;
  profileSharing: boolean;
}
export interface ProfileAvatar {
  data: object;
  contentType: string;
  size: number;
}
export interface Avatar {
  data: object;
  contentType: string;
  length: number;
}

class ConversationList extends React.PureComponent<Props, {}> {
  render() {
    return (
      <div>
        {this.props.items.map((item, index) =>
          <ConversationListItem
            key={`ConversationListItem-${item.cid}`}
            id={item.cid}
            isSelected={index % 10 === 0}
            avatarURL={item.avatarUrl}
            name={item.get('name')}
          />
        )}
      </div>
    );
  }
}

// WORKAROUND: Expose React component on global `window.Whisper.React`:
window.Whisper = window.Whisper || {};
window.Whisper.React = window.Whisper.React || {};
window.Whisper.React.ConversationList = ConversationList;
