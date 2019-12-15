#include "utility.hpp"
#include <functional>
#include <cstddef>
#include "exception.hpp"
#include <cstdio>
#include <iostream>
#include <fstream>

namespace sjtu {
    constexpr char BPTREE_ADDRESS[128] = "btree.lyx";

    template<class Key, class Value, class Compare = std::less <Key> >
    class BTree {
    private:
        // Your private members go here
        struct BptNode {
            //类型:normal or leaf
            bool node_type = false;
            off_t sz = 0;
            //位置
            off_t node_pos = 0;
            off_t father = 0;
            off_t prevNode = 0;
            off_t nextNode = 0;
        };

        struct Normal_Data_Node {
            off_t _child = 0;
            Key k = Key();
        };

        //整个节点的大小
        constexpr static off_t NODE_SIZE = 4096;
        //预留节点的大小
        constexpr static off_t INIT_SIZE = sizeof(BptNode);
        constexpr static off_t KEY_SIZE = sizeof(Key);
        constexpr static off_t VALUE_SIZE = sizeof(Value);
        //节点能够存储的孩子个数M
        constexpr static off_t NODE_KEY_NUM = (NODE_SIZE - INIT_SIZE) / sizeof(Normal_Data_Node) - 1;
        //数据块存储的记录个数L
        constexpr static off_t NODE_PAIR_NUM = (NODE_SIZE - INIT_SIZE) / (KEY_SIZE + VALUE_SIZE) - 1;

        //文件头
        struct File_Head {
            off_t node_cnt = 1;
            off_t root_pos = 0;
            off_t head_node = 0;
            off_t rear_node = 0;
            off_t sz = 0;
        };

        //节点
        struct Normal_Data {
            Normal_Data_Node val[NODE_KEY_NUM];
        };

        //叶节点/数据块
        struct Leaf_Data {
            pair<Key, Value> val[NODE_PAIR_NUM];
        };

        //文件头
        File_Head tree;

        //文件指针
        static FILE *file;

        //写入基本数据
        void write_data() {
            fseek(file, 0, 0);
            char buff[NODE_SIZE] = {0};
            memcpy(buff, &tree, sizeof(tree));
            mem_write(buff, NODE_SIZE, 0);
        }

        //内存读取
        template<class MEM_TYPE>
        static void mem_read(MEM_TYPE buff, off_t buff_size, off_t pos) {
            fseek(file, long(buff_size * pos), 0);
            fread(buff, buff_size, 1, file);
        }

        //内存写入
        template<class MEM_TYPE>
        static void mem_write(MEM_TYPE buff, off_t buff_size, off_t pos) {
            fseek(file, long(buff_size * pos), 0);
            fwrite(buff, buff_size, 1, file);
            fflush(file);
        }

        //获取新内存
        off_t mem_allocation() {
            tree.node_cnt++;
            write_data();
            char buff[NODE_SIZE] = {0};
            mem_write(buff, NODE_SIZE, tree.node_cnt - 1);
            return tree.node_cnt - 1;
        }

        //创建索引节点
        off_t create_normal_node(off_t father) {
            auto node_pos = mem_allocation();
            BptNode tmp;
            Normal_Data normal_data;
            tmp.node_type = false;
            tmp.father = father;
            tmp.node_pos = node_pos;
            tmp.sz = 0;
            write_node(&tmp, &normal_data, node_pos);
            return node_pos;
        }

        //创建叶子节点
        off_t create_leaf_node(off_t father, off_t prev, off_t next) {
            off_t node_pos = mem_allocation();
            BptNode tmp;
            Leaf_Data leaf_data;
            tmp.node_type = true;
            tmp.father = father;
            tmp.node_pos = node_pos;
            tmp.prevNode = prev;
            tmp.nextNode = next;
            tmp.sz = 0;
            write_node(&tmp, &leaf_data, node_pos);
            return node_pos;
        }

        //写入节点信息
        template<class DATA_TYPE>
        static void write_node(BptNode *it, DATA_TYPE *data, off_t pos) {
            char buff[NODE_SIZE] = {0};
            memcpy(buff, it, sizeof(BptNode));
            memcpy(buff + INIT_SIZE, data, sizeof(DATA_TYPE));
            mem_write(buff, NODE_SIZE, pos);
        }

        //读取节点信息
        template<class DATA_TYPE>
        static void read_node(BptNode *it, DATA_TYPE *data, off_t pos) {
            char buff[NODE_SIZE] = {0};
            mem_read(buff, NODE_SIZE, pos);
            memcpy(it, buff, sizeof(BptNode));
            memcpy(data, buff + INIT_SIZE, sizeof(DATA_TYPE));
        }

        //索引节点插入新索引
        void insert_new_index(BptNode &father, Normal_Data &father_data,
                              off_t origin, off_t new_pos, const Key &new_index) {
            auto p = father.sz - 1;
            father.sz++;
           while(father_data.val[p]._child != origin) {
                father_data.val[p + 1] = father_data.val[p];
                p--;
            }
            father_data.val[p + 1].k = father_data.val[p].k;
            father_data.val[p].k = new_index;
            father_data.val[p + 1]._child = new_pos;
        }

        //分裂叶子节点
        Key split_leaf(off_t pos, BptNode &origin, Leaf_Data &origin_data) {
            //读入数据
            off_t father_pos;
            BptNode father;
            Normal_Data father_data;

            //判断是否为根节点
            if (pos == tree.root_pos) {
                //创建根节点
                auto root_pos = create_normal_node(0);
                tree.root_pos = root_pos;
                write_data();
                read_node(&father, &father_data, root_pos);
                origin.father = root_pos;
                father.sz++;
                father_data.val[0]._child = pos;
                father_pos = root_pos;
            } else     //不是根节点
            {
                read_node(&father, &father_data, origin.father);
                father_pos = father.node_pos;
            }
            if (spilt_father(origin)) {
                father_pos = origin.father;
                read_node(&father, &father_data, father_pos);
            }
            //创建一个新的子节点
            auto new_pos = create_leaf_node(father_pos, pos, origin.nextNode);

            //修改后继节点的前驱
            auto tmp_pos = origin.nextNode;//原来的next的位置
            BptNode tmp;
            Leaf_Data tmp_data;
            read_node(&tmp, &tmp_data, tmp_pos);//读取原来的next为tmp
            tmp.prevNode = new_pos;
            write_node(&tmp, &tmp_data, tmp_pos);//把新节点写入原来的位置
            origin.nextNode = new_pos;

            //读取新节点
            BptNode new_node;
            Leaf_Data new_data;
            read_node(&new_node, &new_data, new_pos);

            //移动数据的位置,origin后一半移入新节点
            off_t mid = origin.sz >> 1;
            for (off_t p = mid, i = 0; p < origin.sz; ++p, ++i) {
                new_data.val[i].first = origin_data.val[p].first;
                new_data.val[i].second = origin_data.val[p].second;
                new_node.sz++;
            }
            origin.sz = mid;
            //插入新节点的索引
            insert_new_index(father, father_data, pos, new_pos, origin_data.val[mid].first);

            //写入数据
            write_node(&origin, &origin_data, pos);
            write_node(&new_node, &new_data, new_pos);
            write_node(&father, &father_data, father_pos);

            return new_data.val[0].first;
        }

        //分裂父亲（返回新的父亲）
        bool spilt_father(BptNode &child) {
            //读取数据
            BptNode father, origin;
            off_t father_pos, origin_pos = child.father;
            Normal_Data father_data, origin_data;
            read_node(&origin, &origin_data, origin_pos);
            if (origin.sz < NODE_KEY_NUM)
                return false;//无需分裂

            //判断是否为根节点
            if (origin_pos == tree.root_pos) {
                //创建一个新的根节点
                off_t root_pos = create_normal_node(0);
                tree.root_pos = root_pos;
                write_data();
                read_node(&father, &father_data, root_pos);//读取新的根节点
                origin.father = root_pos;
                father.sz=1;
                father_data.val[0]._child = origin_pos;
                father_pos = root_pos;
            } else    //不是根节点
            {
                read_node(&father, &father_data, origin.father);
                father_pos = father.node_pos;
            }
            if (spilt_father(origin)) {
                father_pos = origin.father;
                read_node(&father, &father_data, father_pos);
            }
            //创建一个新的子节点
            off_t new_pos = create_normal_node(father_pos);
            BptNode new_node;
            Normal_Data new_data;
            read_node(&new_node, &new_data, new_pos);

            //移动数据的位置(后一半移入新节点)
            off_t mid = origin.sz >> 1;
            for (off_t p = mid + 1, i = 0; p < origin.sz; ++p, ++i) {
                if (origin_data.val[p]._child == child.node_pos) {
                    child.father = new_pos;
                }//换新的父亲
                new_data.val[i]=origin_data.val[p];
                new_node.sz++;
            }
            origin.sz = mid + 1;
            //插入新节点的索引
            insert_new_index(father, father_data, origin_pos, new_pos, origin_data.val[mid].k);

            //写入数据
            write_node(&origin, &origin_data, origin_pos);
            write_node(&new_node, &new_data, new_pos);
            write_node(&father, &father_data, father_pos);
            return true;
        }

        //修改索引
        void change_index(off_t l_father, off_t l_child, const Key &new_key) {
            //读取父节点
            BptNode father;
            Normal_Data father_data;
            read_node(&father, &father_data, l_father);
            if (father_data.val[father.sz - 1]._child == l_child) {
                change_index(father.father, l_father, new_key);//往上修改
                return;
            }
            off_t i = father.sz - 2;
            while (father_data.val[i]._child != l_child) {
                i--;
            }
            father_data.val[i].k = new_key;
            //写入父节点
            write_node(&father, &father_data, l_father);
        }

        //合并索引
        void merge_normal(BptNode &l, Normal_Data &l_data, BptNode &r, Normal_Data &r_data) {
            for (off_t p = l.sz, i = 0; i < r.sz; ++p, ++i) {
                l_data.val[p] = r_data.val[i];
            }//r的数据移入l中
            l_data.val[l.sz - 1].k = adjust_normal(r.father, r.node_pos);
            l.sz += r.sz;
            write_node(&l, &l_data, l.node_pos);
        }

        //平衡索引
        void balance_normal(BptNode &it, Normal_Data &it_data) {
            if (it.sz >= NODE_KEY_NUM / 2) {
                write_node(&it, &it_data, it.node_pos);
                return;
            }//数量正常无需修改
            //判断是否为根
            if (it.node_pos == tree.root_pos && it.sz <= 1) {
                tree.root_pos = it_data.val[0]._child;//根节点的儿子<=1，根节点多余了
                write_data();
                return;
            } else if (it.node_pos == tree.root_pos) {
                write_node(&it, &it_data, it.node_pos);
                return;
            }//根节点也无需改变
            //获取兄弟
            BptNode father, brother;
            Normal_Data father_data, brother_data;
            read_node(&father, &father_data, it.father);
            off_t i = 0;
            while (father_data.val[i]._child != it.node_pos) { i++; }//it在父节点中的位置
            if (i > 0)  //找前面的兄弟
            {
                read_node(&brother, &brother_data, father_data.val[i - 1]._child);
                brother.father = it.father;
                if (brother.sz > NODE_KEY_NUM / 2)  //兄弟节点孩子数大于一半，领养一个孩子
                {
                    for (off_t p = it.sz; p > 0; --p) {
                        it_data.val[p] = it_data.val[p - 1];
                    }//往后移一个位置
                    it_data.val[0]._child = brother_data.val[brother.sz - 1]._child;//领养一个孩子
                    it_data.val[0].k = father_data.val[i - 1].k;
                    father_data.val[i - 1].k = brother_data.val[brother.sz - 2].k;//修改两处的key
                    brother.sz--;
                    it.sz++;
                    //写入三个节点
                    write_node(&it, &it_data, it.node_pos);
                    write_node(&brother, &brother_data, brother.node_pos);
                    write_node(&father, &father_data, father.node_pos);
                    return;
                } else {
                    merge_normal(brother, brother_data, it, it_data);//兄弟节点孩子数小于一半，直接合并个节点
                    return;
                }
            }
            if (i < father.sz - 1)  //还没有满，找后面的兄弟
            {
                read_node(&brother, &brother_data, father_data.val[i + 1]._child);
                brother.father = it.father;
                if (brother.sz > NODE_KEY_NUM / 2)  //兄弟节点孩子数大于一半，领养一个孩子
                {
                    it_data.val[it.sz]._child = brother_data.val[0]._child;//领养一个孩子
                    it_data.val[it.sz - 1].k = father_data.val[i].k;
                    father_data.val[i].k = brother_data.val[0].k;//修改两处的key
                    for (off_t p = 1; p < brother.sz; ++p) {
                        brother_data.val[p - 1] = brother_data.val[p];
                    }//兄弟节点往前移一个
                    brother.sz--;
                    it.sz++;
                    //写入三个节点
                    write_node(&brother, &brother_data, brother.node_pos);
                    write_node(&it, &it_data, it.node_pos);
                    write_node(&father, &father_data, father.node_pos);
                    return;
                } else {
                    merge_normal(it, it_data, brother, brother_data);//兄弟节点孩子数小于一半，直接合并个节点
                    return;
                }
            }
        }

        //删除指定的索引（返回关键字）
        Key adjust_normal(off_t pos, off_t removed_child) {
            BptNode it;
            Normal_Data it_data;
            read_node(&it, &it_data, pos);
            off_t i = 0;
            while (it_data.val[i]._child != removed_child)
                ++i;//找到位置
            Key r = it_data.val[i - 1].k;
            it_data.val[i - 1].k = it_data.val[i].k;
            for (; i < it.sz - 1; ++i) {
                it_data.val[i] = it_data.val[i + 1];
            }//后面的前移一个
            it.sz--;
            balance_normal(it, it_data);//平衡索引
            return r;
        }

        //合并叶子
        void merge_leaf(BptNode &l, Leaf_Data &l_data, BptNode &r, Leaf_Data &r_data) {
            for (off_t p = l.sz, i = 0; i < r.sz; ++p, ++i) {
                l_data.val[p].first = r_data.val[i].first;
                l_data.val[p].second = r_data.val[i].second;
            }
            l.sz += r.sz;
            adjust_normal(r.father, r.node_pos);//在r的父节点的索引中删除r
            //修改叶节点的链表
            l.nextNode = r.nextNode;
            BptNode tmp;
            Leaf_Data tmp_data;
            read_node(&tmp, &tmp_data, r.nextNode);//读取r的后继叶节点
            tmp.prevNode = l.node_pos;
            //写入修改的两个节点
            write_node(&tmp, &tmp_data, tmp.node_pos);
            write_node(&l, &l_data, l.node_pos);
        }

        //平衡叶子
        void balance_leaf(BptNode &it, Leaf_Data &it_data) {
            if (it.sz >= NODE_PAIR_NUM / 2) {
                write_node(&it, &it_data, it.node_pos);
                return;
            }//数量正常无需修改
                //判断是否为根
            else if (it.node_pos == tree.root_pos && it.sz == 0)  //树为空
            {
                BptNode tmp;
                Leaf_Data tmp_data;
                read_node(&tmp, &tmp_data, tree.head_node);//读取头节点
                tmp.nextNode = tree.rear_node;//头尾相连
                write_node(&tmp, &tmp_data, tree.head_node);
                read_node(&tmp, &tmp_data, tree.rear_node);//读取尾节点
                tmp.prevNode = tree.head_node;//头尾相连
                write_node(&tmp, &tmp_data, tree.rear_node);
                return;
            } else if (it.node_pos == tree.root_pos) {
                write_node(&it, &it_data, it.node_pos);
                return;
            }//根节点无需改变
            //获取兄弟
            BptNode father, brother;
            Normal_Data father_data;
            Leaf_Data brother_data;
            read_node(&father, &father_data, it.father);
            off_t i = 0;
            for (; i < father.sz; ++i) {
                if (father_data.val[i]._child == it.node_pos)
                    break;
            }
            if (i > 0)  //找前面的兄弟
            {
                read_node(&brother, &brother_data, it.prevNode);
                brother.father = it.father;
                if (brother.sz > NODE_PAIR_NUM / 2)  //兄弟节点孩子数大于一半，领养一个孩子
                {
                    for (off_t p = it.sz; p > 0; --p) {
                        it_data.val[p].first = it_data.val[p - 1].first;
                        it_data.val[p].second = it_data.val[p - 1].second;
                    }//往后移一个位置
                    it_data.val[0].first = brother_data.val[brother.sz - 1].first;//领养一个孩子
                    it_data.val[0].second = brother_data.val[brother.sz - 1].second;
                    it.sz++;
                    brother.sz--;
                    change_index(brother.father, brother.node_pos, it_data.val[0].first);//修改兄弟父亲的索引
                    write_node(&it, &it_data, it.node_pos);
                    write_node(&brother, &brother_data, brother.node_pos);
                    return;
                } else {
                    merge_leaf(brother, brother_data, it, it_data);//兄弟节点孩子数小于一半，直接合并个节点
                    return;
                }
            }
            if (i < father.sz - 1)  //还没有满，找后面的兄弟
            {
                read_node(&brother, &brother_data, it.nextNode);
                brother.father = it.father;
                if (brother.sz > NODE_PAIR_NUM / 2)  //兄弟节点孩子数大于一半，领养一个孩子
                {
                    it_data.val[it.sz].first = brother_data.val[0].first;//领养一个孩子
                    it_data.val[it.sz].second = brother_data.val[0].second;
                    for (off_t p = 1; p < brother.sz; ++p) {
                        brother_data.val[p - 1].first = brother_data.val[p].first;
                        brother_data.val[p - 1].second = brother_data.val[p].second;
                    }//兄弟节点往前移一个
                    it.sz++;
                    brother.sz--;
                    change_index(it.father, it.node_pos, brother_data.val[0].first);
                    write_node(&it, &it_data, it.node_pos);
                    write_node(&brother, &brother_data, brother.node_pos);
                    return;
                } else {
                    merge_leaf(it, it_data, brother, brother_data);//兄弟节点孩子数小于一半，直接合并个节点
                    return;
                }
            }
        }

        //创建文件
        void check_file() {
            if (!file) {
                //创建新的树
                file = fopen(BPTREE_ADDRESS, "wb+");
                write_data();
                //创建两个叶节点
                auto head = tree.node_cnt, rear = tree.node_cnt + 1;
                tree.head_node = head;
                tree.rear_node = rear;
                create_leaf_node(0, 0, rear);
                create_leaf_node(0, head, 0);
                return;
            }
            char buff[NODE_SIZE] = {0};
            mem_read(buff, NODE_SIZE, 0);
            memcpy(&tree, buff, sizeof(tree));
        }

    public:
        typedef pair<const Key, Value> value_type;

        class const_iterator;

        class iterator {
            friend class sjtu::BTree<Key, Value, Compare>::const_iterator;

            friend iterator sjtu::BTree<Key, Value, Compare>::begin();

            friend iterator sjtu::BTree<Key, Value, Compare>::end();

            friend iterator sjtu::BTree<Key, Value, Compare>::find(const Key &);

            friend pair<iterator, OperationResult> sjtu::BTree<Key, Value, Compare>::insert(const Key &, const Value &);

        private:
            // Your private members go here
            //储存当前数据
            BTree *i_BPT = nullptr;
            BptNode i_node;
            off_t i_pos = 0;
        public:
            bool modify(const Value &value) {
                BptNode it;
                Leaf_Data it_data;
                read_node(&it, &it_data, i_node.node_pos);
                it_data.val[i_pos].second = value;
                write_node(&it, &it_data, it.node_pos);
                return true;
            }

            iterator() {
                // TODO Default Constructor
            }

            iterator(const iterator &other) {
                // TODO Copy Constructor
                i_BPT = other.i_BPT;
                i_node = other.i_node;
                i_pos = other.i_pos;
            }

            // Return a new iterator which points to the n-next elements
            iterator operator++(int) {
                // Todo iterator++
                auto tmp = *this;
                if (i_pos >= i_node.sz - 1) {
                    char buff[NODE_SIZE] = {0};
                    mem_read(buff, NODE_SIZE, i_node.nextNode);
                    memcpy(&i_node, buff, sizeof(i_node));
                    i_pos = 0;
                } else
                    i_pos++;
                return tmp;
            }

            iterator &operator++() {
                // Todo ++iterator
                if (i_pos >= i_node.sz - 1) {
                    char buff[NODE_SIZE] = {0};
                    mem_read(buff, NODE_SIZE, i_node.nextNode);
                    memcpy(&i_node, buff, sizeof(i_node));
                    i_pos = 0;
                } else
                    i_pos++;
                return *this;
            }

            iterator operator--(int) {
                // Todo iterator--
                auto tmp = *this;
                if (i_pos <= 0) {
                    char buff[NODE_SIZE] = {0};
                    mem_read(buff, NODE_SIZE, i_node.prevNode);
                    memcpy(&i_node, buff, sizeof(i_node));
                    i_pos = i_node.sz - 1;
                } else
                    i_pos--;
                return tmp;
            }

            iterator &operator--() {
                // Todo --iterator
                if (i_pos <= 0) {
                    char buff[NODE_SIZE] = {0};
                    mem_read(buff, NODE_SIZE, i_node.prevNode);
                    memcpy(&i_node, buff, sizeof(i_node));
                    i_pos = i_node.sz - 1;
                } else
                    i_pos--;
                return *this;
            }

            // Overloaded of operator '==' and '!='
            // Check whether the iterators are same
            bool operator==(const iterator &rhs) const {
                // Todo operator ==
                return i_BPT == rhs.i_BPT && i_node.node_pos == rhs.i_node.node_pos
                       && i_pos == rhs.i_pos;
            }

            bool operator==(const const_iterator &rhs) const {
                // Todo operator ==
                return i_BPT == rhs.i_BPT && i_node.node_pos == rhs.i_node.node_pos
                       && i_pos == rhs.i_pos;
            }

            bool operator!=(const iterator &rhs) const {
                // Todo operator !=
                return i_BPT != rhs.i_BPT || i_node.node_pos != rhs.i_node.node_pos
                       || i_pos != rhs.i_pos;
            }

            bool operator!=(const const_iterator &rhs) const {
                // Todo operator !=
                return i_BPT != rhs.i_BPT || i_node.node_pos != rhs.i_node.node_pos
                       || i_pos != rhs.i_pos;
            }
        };

        class const_iterator {
            // it should has similar member method as iterator.
            //  and it should be able to construct from an iterator.
            friend class sjtu::BTree<Key, Value, Compare>::iterator;

            friend const_iterator sjtu::BTree<Key, Value, Compare>::cbegin() const;

            friend const_iterator sjtu::BTree<Key, Value, Compare>::cend() const;

            friend const_iterator sjtu::BTree<Key, Value, Compare>::find(const Key &) const;

        private:
            // Your private members go here
            //储存当前数据
            BTree *i_BPT = nullptr;
            BptNode i_node;
            off_t i_pos = 0;
        public:
            const_iterator() {
                // TODO
            }

            const_iterator(const const_iterator &other) {
                // TODO
                i_BPT = other.i_BPT;
                i_node = other.i_node;
                i_pos = other.i_pos;
            }

            const_iterator(const iterator &other) {
                // TODO
                i_BPT = other.i_BPT;
                i_node = other.i_node;
                i_pos = other.i_pos;
            }

            // And other methods in iterator, please fill by yourself.
            // Return a new iterator which points to the n-next elements
            const_iterator operator++(int) {
                // Todo iterator++
                auto tmp = *this;
                if (i_pos >= i_node.sz - 1) {
                    char buff[NODE_SIZE] = {0};
                    mem_read(buff, NODE_SIZE, i_node.nextNode);
                    memcpy(&i_node, buff, sizeof(i_node));
                    i_pos = 0;
                } else
                    i_pos++;
                return tmp;
            }

            const_iterator &operator++() {
                // Todo ++iterator
                if (i_pos >= i_node.sz - 1) {
                    char buff[NODE_SIZE] = {0};
                    mem_read(buff, NODE_SIZE, i_node.nextNode);
                    memcpy(&i_node, buff, sizeof(i_node));
                    i_pos = 0;
                } else
                    i_pos++;
                return *this;
            }

            const_iterator operator--(int) {
                // Todo iterator--
                auto tmp = *this;
                if (i_pos <= 0) {
                    char buff[NODE_SIZE] = {0};
                    mem_read(buff, NODE_SIZE, i_node.prevNode);
                    memcpy(&i_node, buff, sizeof(i_node));
                    i_pos = i_node.sz - 1;
                } else
                    i_pos--;
                return tmp;
            }

            const_iterator &operator--() {
                // Todo --iterator
                if (i_pos <= 0) {
                    char buff[NODE_SIZE] = {0};
                    mem_read(buff, NODE_SIZE, i_node.prevNode);
                    memcpy(&i_node, buff, sizeof(i_node));
                    i_pos = i_node.sz - 1;
                } else
                    i_pos--;
                return *this;
            }

            // Overloaded of operator '==' and '!='
            // Check whether the iterators are same
            bool operator==(const iterator &rhs) const {
                // Todo operator ==
                return i_BPT == rhs.i_BPT && i_node.node_pos == rhs.i_node.node_pos
                       && i_pos == rhs.i_pos;
            }

            bool operator==(const const_iterator &rhs) const {
                // Todo operator ==
                return i_BPT == rhs.i_BPT && i_node.node_pos == rhs.i_node.node_pos
                       && i_pos == rhs.i_pos;
            }

            bool operator!=(const iterator &rhs) const {
                // Todo operator !=
                return i_BPT != rhs.i_BPT || i_node.node_pos != rhs.i_node.node_pos
                       || i_pos != rhs.i_pos;
            }

            bool operator!=(const const_iterator &rhs) const {
                // Todo operator !=
                return i_BPT != rhs.i_BPT || i_node.node_pos != rhs.i_node.node_pos
                       || i_pos != rhs.i_pos;
            }
        };

        // Default Constructor and Copy Constructor
        BTree() {
            // Todo Default
            file = fopen(BPTREE_ADDRESS, "rb+");
            if (!file) {
                //创建新的树
                file = fopen(BPTREE_ADDRESS, "wb+");
                write_data();
                //创建两个叶节点
                auto head = tree.node_cnt, rear = tree.node_cnt + 1;
                tree.head_node = head;
                tree.rear_node = rear;
                create_leaf_node(0, 0, rear);
                create_leaf_node(0, head, 0);
                return;
            }
            char buff[NODE_SIZE] = {0};
            mem_read(buff, NODE_SIZE, 0);
            memcpy(&tree, buff, sizeof(tree));
        }

        BTree(const BTree &other) {
            // Todo Copy
            file = fopen(BPTREE_ADDRESS, "rb+");
            tree.node_cnt = other.tree.node_cnt;
            tree.root_pos = other.tree.root_pos;
            tree.head_node = other.tree.head_node;
            tree.rear_node = other.tree.rear_node;
            tree.sz = other.tree.sz;
        }

        BTree &operator=(const BTree &other) {
            // Todo Assignment
            file = fopen(BPTREE_ADDRESS, "rb+");
            tree.node_cnt = other.tree.node_cnt;
            tree.root_pos = other.tree.root_pos;
            tree.head_node = other.tree.head_node;
            tree.rear_node = other.tree.rear_node;
            tree.sz = other.tree.sz;
            return *this;
        }

        ~BTree() {
            // Todo Destructor
            fclose(file);
        }

        // Insert: Insert certain Key-Value into the database
        // Return a pair, the first of the pair is the iterator point to the new
        // element, the second of the pair is Success if it is successfully inserted
        pair<iterator, OperationResult> insert(const Key &key, const Value &value) {
            check_file();
            //树为空
            if (empty()) {
                //创建新的叶节点
                auto root_pos = create_leaf_node(0, tree.head_node, tree.rear_node);
                BptNode tmp;
                Leaf_Data tmp_data;
                //修改头节点
                read_node(&tmp, &tmp_data, tree.head_node);
                tmp.nextNode = root_pos;
                write_node(&tmp, &tmp_data, tree.head_node);
                //修改尾节点
                read_node(&tmp, &tmp_data, tree.rear_node);
                tmp.prevNode = root_pos;
                write_node(&tmp, &tmp_data, tree.rear_node);
                //写入数据到节点
                read_node(&tmp, &tmp_data, root_pos);
                tmp.sz = 1;
                tmp_data.val[0].first = key;
                tmp_data.val[0].second = value;
                write_node(&tmp, &tmp_data, root_pos);
                //
                tree.sz++;
                tree.root_pos = root_pos;
                write_data();

                pair<iterator, OperationResult> result(begin(), Success);
                return result;
            }
            //查找节点位置
            char buff[NODE_SIZE] = {0};
            off_t cur_pos = tree.root_pos, father = 0;
            while (true) {
                mem_read(buff, NODE_SIZE, cur_pos);
                BptNode tmp;
                memcpy(&tmp, buff, sizeof(tmp));
                //判断父亲是否更新
                if (father != tmp.father) {
                    tmp.father = father;
                    memcpy(buff, &tmp, sizeof(tmp));
                    mem_write(buff, NODE_SIZE, cur_pos);
                }
                if (tmp.node_type)
                    break;//叶节点
                Normal_Data normal_data;
                memcpy(&normal_data, buff + INIT_SIZE, sizeof(normal_data));
                off_t child_pos = tmp.sz - 1;
                for (; child_pos > 0; --child_pos) {
                    if (!(normal_data.val[child_pos - 1].k > key))
                        break;
                }
                father = cur_pos;
                cur_pos = normal_data.val[child_pos]._child;
            }

            BptNode it;
            memcpy(&it, buff, sizeof(it));
            Leaf_Data it_data;
            memcpy(&it_data, buff + INIT_SIZE, sizeof(it_data));
            for (off_t val_pos = 0;; ++val_pos) {
                if (val_pos < it.sz && (!(it_data.val[val_pos].first < key || it_data.val[val_pos].first > key)))//已经存在
                    return pair<iterator, OperationResult>(end(), Fail);
                if (val_pos >= it.sz || it_data.val[val_pos].first > key) {
                    //分裂节点
                    if (it.sz >= NODE_PAIR_NUM) {
                        off_t cur_key = split_leaf(cur_pos, it, it_data);
                        //在后面那个
                        if (key > cur_key) {
                            cur_pos = it.nextNode;
                            val_pos -= it.sz;
                            read_node(&it, &it_data, cur_pos);
                        }
                    }
                    //后面的往后移一个
                    for (off_t p = it.sz - 1; p >= val_pos; --p) {
                        it_data.val[p + 1].first = it_data.val[p].first;
                        it_data.val[p + 1].second = it_data.val[p].second;
                        if (p == val_pos)break;
                    }
                    it_data.val[val_pos].first = key;
                    it_data.val[val_pos].second = value;
                    it.sz++;
                    write_node(&it, &it_data, cur_pos);
                    iterator ans;
                    ans.i_BPT = this;
                    ans.i_node = it;
                    ans.i_pos = val_pos;
                    //修改树的参数
                    tree.sz++;
                    write_data();
                    pair<iterator, OperationResult> result(ans, Success);
                    return result;
                }
            }
        }

        // Erase: Erase the Key-Value
        // Return Success if it is successfully erased
        // Return Fail if the key doesn't exist in the database
        OperationResult erase(const Key &key) {
            // TODO erase function
            check_file();
            if (empty())
                return Fail;

            //查找节点位置
            char buff[NODE_SIZE] = {0};
            off_t cur_pos = tree.root_pos, father = 0;
            while (true) {
                mem_read(buff, NODE_SIZE, cur_pos);
                BptNode tmp;
                memcpy(&tmp, buff, sizeof(tmp));
                //判断父亲是否更新
                if (father != tmp.father) {
                    tmp.father = father;
                    memcpy(buff, &tmp, sizeof(tmp));
                    mem_write(buff, NODE_SIZE, cur_pos);
                }
                if (tmp.node_type)
                    break;//叶节点
                Normal_Data normal_data;
                memcpy(&normal_data, buff + INIT_SIZE, sizeof(normal_data));
                off_t child_pos = tmp.sz - 1;
                for (; child_pos > 0; --child_pos) {
                    if (!(normal_data.val[child_pos - 1].k > key))
                        break;
                }
                father = cur_pos;
                cur_pos = normal_data.val[child_pos]._child;
            }
            BptNode it;
            memcpy(&it, buff, sizeof(it));
            Leaf_Data it_data;
            memcpy(&it_data, buff + INIT_SIZE, sizeof(it_data));
            for (off_t val_pos = 0;; ++val_pos) {
                if (val_pos < it.sz && (!(it_data.val[val_pos].first < key || it_data.val[val_pos].first > key)))  //存在
                {
                    it.sz--;
                    //后面的往前移一个
                    for (off_t p = val_pos; p < it.sz; ++p) {
                        it_data.val[p].first = it_data.val[p + 1].first;
                        it_data.val[p].second = it_data.val[p + 1].second;
                    }
                    balance_leaf(it, it_data);
                    //修改树的数据
                    tree.sz--;
                    write_data();
                    return Success;
                }
                if (val_pos >= it.sz || it_data.val[val_pos].first > key)
                    return Fail;
            }
        }

        // Return a iterator to the beginning
        iterator begin() {
            check_file();
            iterator i;
            char buff[NODE_SIZE] = {0};
            mem_read(buff, NODE_SIZE, tree.head_node);
            BptNode head;
            memcpy(&head, buff, sizeof(head));
            i.i_node = head;
            i.i_BPT = this;
            i.i_pos = 0;
            i++;
            return i;
        }

        const_iterator cbegin() const {
            const_iterator i;
            char buff[NODE_SIZE] = {0};
            mem_read(buff, NODE_SIZE, tree.head_node);
            BptNode head;
            memcpy(&head, buff, sizeof(head));
            i.i_node = head;
            i.i_BPT = this;
            i.i_pos = 0;
            i++;
            return i;
        }

        // Return a iterator to the end(the next element after the last)
        iterator end() {
            check_file();
            iterator i;
            char buff[NODE_SIZE] = {0};
            mem_read(buff, NODE_SIZE, tree.rear_node);
            BptNode rear;
            memcpy(&rear, buff, sizeof(rear));
            i.i_node = rear;
            i.i_BPT = this;
            i.i_pos = 0;
            return i;
        }

        const_iterator cend() const {
            const_iterator i;
            char buff[NODE_SIZE] = {0};
            mem_read(buff, NODE_SIZE, tree.rear_node);
            BptNode rear;
            memcpy(&rear, buff, sizeof(rear));
            i.i_node = rear;
            i.i_BPT = this;
            i.i_pos = 0;
            return i;
        }

        // Check whether this BTree is empty
        bool empty() const {
            if (!file)
                return true;
            return tree.sz == 0;
        }

        // Return the number of <K,V> pairs
        size_t size() const {
            if (!file)
                return 0;
            return tree.sz;
        }

        // Clear the BTree
        void clear() {
            if (!file)
                return;
            remove(BPTREE_ADDRESS);
            file = nullptr;
        }

        // Return the value refer to the Key(key)
        Value at(const Key &key) {
            if (empty())
                throw container_is_empty();
            //查找节点位置
            char buff[NODE_SIZE] = {0};
            off_t cur_pos = tree.root_pos, father = 0;
            while (true) {
                mem_read(buff, NODE_SIZE, cur_pos);
                BptNode tmp;
                memcpy(&tmp, buff, sizeof(tmp));
                //判断父亲是否更新
                if (father != tmp.father) {
                    tmp.father = father;
                    memcpy(buff, &tmp, sizeof(tmp));
                    mem_write(buff, NODE_SIZE, cur_pos);
                }
                if (tmp.node_type)
                    break;//叶节点
                Normal_Data normal_data;
                memcpy(&normal_data, buff + INIT_SIZE, sizeof(normal_data));
                off_t child_pos = tmp.sz - 1;
                for (; child_pos > 0; --child_pos) {
                    if (!(normal_data.val[child_pos - 1].k > key))
                        break;
                }
                father = cur_pos;
                cur_pos = normal_data.val[child_pos]._child;
            }
            BptNode it;
            memcpy(&it, buff, sizeof(it));
            Leaf_Data it_data;
            memcpy(&it_data, buff + INIT_SIZE, sizeof(it_data));
            for (off_t val_pos = 0;; ++val_pos) {
                if (val_pos < it.sz && (!(it_data.val[val_pos].first < key ||
                                          it_data.val[val_pos].first > key))) { return it_data.val[val_pos].second; }
                if (val_pos >= it.sz || it_data.val[val_pos].first > key) { throw index_out_of_bound(); }
            }
        }

        /**
         * Returns the number of elements with key
         *   that compares equivalent to the specified argument,
         * The default method of check the equivalence is !(a < b || b > a)
         */
        size_t count(const Key &key) const {
            if (find(key) == cend())
                return 0;
            else
                return 1;
        }

        /**
         * Finds an element with key equivalent to key.
         * key value of the element to search for.
         * Iterator to an element with key equivalent to key.
         *   If no such element is found, past-the-end (see end()) iterator is
         * returned.
         */
        iterator find(const Key &key) {
            if (empty())
                return end();
            //查找节点位置
            char buff[NODE_SIZE] = {0};
            off_t cur_pos = tree.root_pos, father = 0;
            while (true) {
                mem_read(buff, NODE_SIZE, cur_pos);
                BptNode tmp;
                memcpy(&tmp, buff, sizeof(tmp));
                //判断父亲是否更新
                if (father != tmp.father) {
                    tmp.father = father;
                    memcpy(buff, &tmp, sizeof(tmp));
                    mem_write(buff, NODE_SIZE, cur_pos);
                }
                if (tmp.node_type)
                    break;//叶节点
                Normal_Data normal_data;
                memcpy(&normal_data, buff + INIT_SIZE, sizeof(normal_data));
                off_t child_pos = tmp.sz - 1;
                for (; child_pos > 0; --child_pos) {
                    if (!(normal_data.val[child_pos - 1].k > key))
                        break;
                }
                father = cur_pos;
                cur_pos = normal_data.val[child_pos]._child;
            }
            BptNode it;
            memcpy(&it, buff, sizeof(it));
            Leaf_Data it_data;
            memcpy(&it_data, buff + INIT_SIZE, sizeof(it_data));
            for (off_t val_pos = 0;; ++val_pos) {
                if (val_pos < it.sz && !(it_data.val[val_pos].first < key || it_data.val[val_pos].first > key)) {
                    iterator result;
                    result.i_BPT = this;
                    result.i_node = it;
                    result.i_pos = val_pos;
                    return result;
                }
                if (val_pos >= it.sz || it_data.val[val_pos].first > key)
                    return end();
            }
        }

        const_iterator find(const Key &key) const {
            if (empty())
                return cend();
            //查找节点位置
            char buff[NODE_SIZE] = {0};
            off_t cur_pos = tree.root_pos, father = 0;
            while (true) {
                mem_read(buff, NODE_SIZE, cur_pos);
                BptNode tmp;
                memcpy(&tmp, buff, sizeof(tmp));
                //判断父亲是否更新
                if (father != tmp.father) {
                    tmp.father = father;
                    memcpy(buff, &tmp, sizeof(tmp));
                    mem_write(buff, NODE_SIZE, cur_pos);
                }
                if (tmp.node_type)
                    break;//叶节点
                Normal_Data normal_data;
                memcpy(&normal_data, buff + INIT_SIZE, sizeof(normal_data));
                off_t child_pos = tmp.sz - 1;
                for (; child_pos > 0; --child_pos) {
                    if (!(normal_data.val[child_pos - 1].k > key))
                        break;
                }
                father = cur_pos;
                cur_pos = normal_data.val[child_pos]._child;
            }
            BptNode it;
            memcpy(&it, buff, sizeof(it));
            Leaf_Data it_data;
            memcpy(&it_data, buff + INIT_SIZE, sizeof(it_data));
            for (off_t val_pos = 0;; ++val_pos) {
                if (val_pos < it.sz && (!(it_data.val[val_pos].first < key || it_data.val[val_pos].first > key))) {
                    const_iterator result;
                    result.i_BPT = this;
                    result.i_node = it;
                    result.i_pos = val_pos;
                    return result;
                }
                if (val_pos >= it.sz || it_data.val[val_pos].first > key)
                    return cend();
            }
        }
    };
    template<class Key, class Value, class Compare>
    FILE *BTree<Key, Value, Compare>::file = nullptr;

} // namespace sjtu

